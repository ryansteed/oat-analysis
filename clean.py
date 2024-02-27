import json
import pandas as pd
import networkx as nx
import click
import yaml
from collections import defaultdict
import re
import unicodedata
import numpy as np
import urllib.request
import os
import sqlalchemy
import github

levels = ["category", "subcategory", "item"]
colls = ["subcategories", "items"]


@click.group('transform')
def transform():
    pass


@transform.command('clean')
@click.argument('path_to_airtable', type=click.Path(exists=True))
@click.argument('out_path', type=str)
def clean(path_to_airtable: str, out_path: str):
    """Clean airtable data for use in analysis and visualization.

    Args:
        path_to_airtable (str): Path to airtable file. Usually `data/airtable.csv`.
        out_path (str): Path to output cleaned data.
    """
    df = _clean(path_to_airtable)
    df.to_csv(out_path, index=False)


def _clean(path_to_airtable):
    print("Parsing from", path_to_airtable, "using taxonomy.json")
    df = pd.read_csv(path_to_airtable)
    df["Taxonomy"] = df["Taxonomy"].astype(str).str.split(",")
    df["homepage_url"], df["repo_url"] = zip(*df["Relevant Links"].apply(_process_urls))
    df["Featured"] = df["Featured"].astype(str)
    df["Logo"] = df["Logo"].astype(str)
    df["Format (TODO)"] = df["Format (TODO)"].str.split(",")
    return df


@transform.command('yaml')
@click.argument('path_to_airtable', type=click.Path(exists=True))
@click.argument('project_path', type=str)
@click.option('--format', type=str, default="custom")
def to_yaml(path_to_airtable: str, project_path: str, format: str = "custom"):
    """Convert airtable to YAML file for visualization.

    Args:
        path_to_airtable (str): Path to airtable file. Usually `data/airtable.csv`.
        project_path (str): Path to directory (usually `oat-landscape`) where YAML file will be deposited.
        format (str, optional): Which format to use. For `oat-landscape` custom format, use `"custom"`.
            For LF landscapeapp, use `"landscapeapp"`. Defaults to "custom".
    """
    if format == "custom":
        print("Using custom format")
        yamler = CustomYAMLer()
    elif format == "landscapeapp":
        yamler = LandscapeAppYAMLer()
    else:
        raise ValueError("Unknown format")
    return yamler.to_yaml(path_to_airtable, project_path)


class YAMLer:
    def to_yaml(self, path_to_airtable, project_path):
        exploded = _transform_for_pivot(path_to_airtable)
        # featured = exploded[~exploded["Featured"].isna()].copy()
        featured = exploded[~exploded[["Taxonomy - 1st", "Taxonomy - 2nd"]].isna().any(axis=1)].copy().sort_values("Featured")
        print("Saving", len(featured), "items to", project_path)

        output = self._output(featured, project_path)

        yamlified = yaml.dump(
            {
                "landscape": self._yamlify(output),
                "count": len(featured),
                "count_unique": len(featured["tool_id"].unique()),
                "update_date": pd.Timestamp.now().strftime("%Y-%m-%d")
            },
            Dumper=CustomDumper,
            sort_keys=False, default_flow_style=False
        )

        yamlified = re.sub(r"organization: '{(.*)}'", r'organization: {\1}', yamlified)
        for level in levels:
            yamlified = re.sub(f'{level}: null', f'{level}:', yamlified)
        with open(self._save_path(project_path), 'w') as f:
            f.write(yamlified)
            f.close()
            print("Saved to", os.path.abspath(self._save_path(project_path)))
        return output

    def _yamlify(self, output):
        return _yamlify_recursive(output)

    def _save_path(self, project_path):
        return f"{project_path}/_data/landscape.yml"
    
    def _accepted_ext(self, ext):
        return True
    
    def _output(self, featured, project_path):
        # mark index to avoid duplicates (TODO allow duplciates, probalby have to modify js)
        featured["id"] = featured["Name"].str.lower().str.strip().str.replace(" ", "-").str.replace("/", "-").str.replace(".", "-").apply(self.slugify)
        featured["Name"] = self._format_names(featured["Name"])
        # add subcategory colors
        palette = ["#230C33", "#CE7B91", "#4A5240", "#594236"]
        colors = {}
        for org, group in featured[~featured["Taxonomy - 3rd"].isna()].groupby("Taxonomy - 2nd"):
            for i, name in enumerate(group["Taxonomy - 3rd"].unique()):
                try:
                    colors[name] = palette[i]
                except IndexError:
                    print("Not enough colors for", name, ", recycling")
                    colors[name] = palette[i % len(palette)]
        colors[None] = None
        featured["color"] = featured["Taxonomy - 3rd"].apply(lambda x: colors[x])

        output = defaultdict(lambda: defaultdict(list))
        for _, group in featured.groupby("tool_id"):
            ind = list(range(len(group)))
            for i in ind:
                row = group.iloc[i]
                others = group.iloc[np.array(ind) != i, :]
                out = self._process_row(row, project_path, others=others if len(group) > 1 else None)
                output[row["Taxonomy - 1st"]][row["Taxonomy - 2nd"]].append(out)

        print("Categories:", output.keys())

        return output

    def _process_row(self, row, project_path, others=None):
        extras = [
            "Format (TODO)",
            "License",
            "Internal/External",
            "Org Type (Creator)",
            # "Taxonomy", # TODO later
            "Audit  Target Type"
        ]
        out = {
            "name": row["Name"],
            "id": row["id"],
            "organization": {"name": row["Organization"]},
            "project": row["Featured"] if row["Featured"] != "nan" else None,
            "taxonomy_first": row["Taxonomy - 1st"],
            "taxonomy_second": row["Taxonomy - 2nd"],
            "taxonomy_third": row["Taxonomy - 3rd"],
            "color": row["color"],
            "tool_id": int(row["tool_id"]),
        }
        out["homepage_url"], out["repo_url"] = _process_urls(row["Relevant Links"])
        out["extra"] = {extra: row[extra] for extra in extras}

        # transfer file
        logo = row["Logo"][row["Logo"].rfind("(")+1:-1]
        logo_dir = f"{project_path}/hosted_logos"
        ext = os.path.splitext(row["Logo"].split("(")[0][:-1])[1]
        if ext == ".":
            ext = ".png"
        logo_name = row["id"] + ext
        out["logo"] = logo_name
        if not os.path.exists(os.path.join(logo_dir, logo_name)):
            try:
                if not self._accepted_ext(ext):
                    raise ValueError("Not an accepted extension")
                urllib.request.urlretrieve(logo, os.path.join(logo_dir, logo_name))
                assert os.path.exists(os.path.join(logo_dir, logo_name))
            except FileNotFoundError as e:
                print("Destination", os.path.abspath(logo_dir), "does not exist")
                raise e
            except ValueError:
                print("Failed to download logo for", row["Name"], "from", row["Logo"])
                out["logo"] = None
        
        return out

    def _format_names(self, names):
        return names.str.replace('\u200C', '')

    @staticmethod
    def slugify(value, allow_unicode=False):
        """
        Taken from https://github.com/django/django/blob/master/django/utils/text.py
        Convert to ASCII if 'allow_unicode' is False. Convert spaces or repeated
        dashes to single dashes. Remove characters that aren't alphanumerics,
        underscores, or hyphens. Convert to lowercase. Also strip leading and
        trailing whitespace, dashes, and underscores.
        """
        value = str(value)
        if allow_unicode:
            value = unicodedata.normalize('NFKC', value)
        else:
            value = unicodedata.normalize('NFKD', value).encode('ascii', 'ignore').decode('ascii')
        value = re.sub(r'[^\w\s-]', '', value.lower())
        return re.sub(r'[-\s]+', '-', value).strip('-_')


class LandscapeAppYAMLer(YAMLer):
    def _accepted_ext(self, ext):
        return ext == "svg"

    def _output(self, featured, project_path):
        output = super()._output(featured, project_path)
        for (org, org_type), group in featured.groupby(["Organization", "Org Type (Creator)"]):
            output["Organizations"][org_type].append({
                "name": org,
                "homepage_url": group["Relevant Links"].astype(str).iloc[0].split(",")[0].strip(), # TODO collect organization URLs
                "organization": f'{{"name": "{org}"}}',
                "logo": "placeholder.svg" # TODO later
            })
        return output

    def _save_path(self, project_path):
        return f"{project_path}/landscape.yml"

    def _format_names(self, names):
        return super()._format_names(names).astype(str) + " (#" + np.arange(len(names)).astype(str) + ")"



class CustomYAMLer(YAMLer):

    def _yamlify(self, output):
        output = super()._yamlify(output)
        # add metadata to categories
        metadata = {
            'Harms Discovery': {"order": 1},
            'Standards Identification & Management': {"order": 2},
            'Transparency Infrastructure': {"order": 3},
            'Data Collection': {"order": 4},
            'Performance Analysis': {"order": 5},
            'Audit Communication': {"order": 6},
            'Advocacy': {"order": 7}
        }
        for entry in output:
            entry.update(metadata[entry["name"]])
        return output

    def _process_row(self, row, project_path, others=None):
        out = super()._process_row(row, project_path)
        out["name_html"] = out["name"]
        if others is not None:
            others = others.to_dict("records")
            print(others)
        out["taxonomies"] = [
            {
                "taxonomy_first": entry["Taxonomy - 1st"],
                "taxonomy_second": entry["Taxonomy - 2nd"],
                "taxonomy_third": entry["Taxonomy - 3rd"]
            } for entry in ([row] if others is None else [row, *others])
        ]
        # generate html for modal
        # TODO: replace with a generator https://jekyllrb.com/docs/plugins/generators/
        html = "---\nlayout: modal-content\n" + yaml.dump(out) + "---"
        path = f"{project_path}/modals/{out['id']}.html"
        if not os.path.exists(os.path.dirname(path)):
            os.makedirs(os.path.dirname(path))
        with open(path, 'wb') as f:
            f.write(html.encode('utf-8'))
        return out



class CustomDumper(yaml.Dumper):
    def increase_indent(self, flow=False, indentless=False):
        return super(CustomDumper, self).increase_indent(flow, False)


def _yamlify_recursive(input, depth=0):
    if isinstance(input, list):
        return [{
            "item": None,
            **i
        } for i in input]
    return [
        {
            levels[depth]: None,
            "name": k,
            colls[depth]: _yamlify_recursive(v, depth=depth+1)
        }
        for k, v in input.items()
    ]


@transform.command('pivot')
@click.argument('path_to_airtable', type=click.Path(exists=True))
@click.option('--crunchbase/--no-crunchbase', default=True)
@click.option('--github/--no-github', default=True)
@click.option('--from-sql', default=False)
def transform_for_pivot(path_to_airtable, crunchbase: bool = True, github: bool = True, from_sql: bool = False):
    exploded = _transform_for_pivot(path_to_airtable)
    if crunchbase:
        # join CB data
        exploded = _join_crunchbase(exploded, "crunchbase", from_sql=from_sql)
    if github:
        # join github data
        exploded = _join_github(exploded)
    exploded.to_csv("output/airtable_for_pivot.csv")
    print("Saved to output/airtable_for_pivot.csv")


def _join_crunchbase(df, path_to_crunchbase, from_sql: bool = False):
    cb_urls = df.loc[df["cb_url"].notna(), "cb_url"]
    cb_data = _crunchbase_csv(path_to_crunchbase, cb_urls) if not from_sql else _crunchbase_sql(path_to_crunchbase, cb_urls)
    missing_rows = [c for c in cb_urls if c not in cb_data["cb_url"].values]
    if len(missing_rows) > 0:
        print("[WARN] Missing {} rows".format(missing_rows))
    df = df.merge(
        cb_data.add_prefix("cb_"),
        left_on="cb_url", right_on="cb_cb_url",
        how="left"
    ).drop(columns=["cb_cb_url"])
    cb_urls_bad_join = df["cb_url"].notna() & df["cb_Organization Name"].isna() & ~df["cb_url"].isin(missing_rows)
    assert cb_urls_bad_join.sum() < 1, "Missing join data for {}".format(df.loc[cb_urls_bad_join, "cb_url"])
    return df


def _crunchbase_sql(path_to_crunchbase, cb_urls):
    print("Querying CB data...")
    cb_urls_formatted = ", ".join([
        "'{}'".format(cb_url)
        for cb_url in cb_urls
    ])
    engine = sqlalchemy.create_engine(f'sqlite:///{path_to_crunchbase}/organizations.db')
    cb_data = pd.read_sql(f"""
        SELECT * FROM organizations
        WHERE cb_url IN ({cb_urls_formatted})
    """, engine)
    return cb_data


def _crunchbase_csv(path_to_crunchbase, cb_urls):
    companies = pd.read_csv(f"{path_to_crunchbase}/cb-query_companies.csv")
    investors = pd.read_csv(f"{path_to_crunchbase}/cb-query_investors.csv")
    schools = pd.read_csv(f"{path_to_crunchbase}/cb-query_schools.csv")
    cb_data = pd.concat(
        [companies, investors, schools],
        keys=["company", "investor", "school"],
        names=["organization_type"]
    ).reset_index("organization_type").rename(columns={"Organization Name URL": "cb_url"})
    cb_data["organization_type"] = pd.Categorical(cb_data["organization_type"], ["company", "school", "investor"])
    # check for duplicate urls
    cb_data = cb_data.sort_values("organization_type").drop_duplicates(subset=["cb_url"], keep="first")
    return cb_data


def _join_github(df):
    gh_exists = df["gh_url"].notna()
    stats = df.loc[gh_exists, "gh_url"].apply(_fetch_gh_repo_stats)
    df = df.merge(stats.add_prefix("gh_"), left_index=True, right_index=True, how="left")
    gh_urls_bad_join = gh_exists & df["gh_owner"].isna()
    assert gh_urls_bad_join.sum() < 1, "Missing join data for {}".format(df.loc[gh_urls_bad_join, "gh_url"])
    return df


def _fetch_gh_repo_stats(repository_url):
    # Author: Victor Ojewale
    # Extract the owner and repository name
    owner, repo_name = repository_url.split('/')[-2:]
    try:
        auth = github.Auth.Token(json.load(open("secrets.json", 'rb'))['github_token'])
        # I think we can get more calls if we use a PAT
        g = github.Github(auth=auth)
        # Get the repository object
        repo = g.get_repo(f"{owner}/{repo_name}")
        # Fetch the number of stars
        stars_count = repo.stargazers_count
        # Fetch the number of forks
        forks_count = repo.forks_count
        # Fetch the number of watchers
        watchers_count = repo.subscribers_count
        # Fetch the number of issues
        issues_count = repo.open_issues_count
        # ONLY WORKS WITH PUSH ACCESS
        # # views
        # views_count = repo.get_views_traffic()
        # # clones
        # clones_count = repo.get_clones_traffic()
        # # top referrers
        # referrers = repo.get_top_referrers()

    except github.GithubException as e:
        print("Exception for repository", repository_url, ":", e)
        return pd.Series([None, None, None, None, None])
    return pd.Series(
        [owner, stars_count, forks_count, watchers_count, issues_count],
        index=["owner", "stars", "forks", "watchers", "issues"]
    )

def _transform_for_pivot(path_to_airtable):
    df = _clean(path_to_airtable)
    taxonomy = json.load(open("data/taxonomy.json", 'rb'))
    tree = nx.DiGraph(taxonomy)
    # pos = nx.drawing.nx_agraph.graphviz_layout(tree, prog='dot', args='-Grankdir=LR -Gnodesep=0.9')
    # nx.draw(tree, pos, with_labels=True, node_size=600, font_size=10)
    
    def find_chains(nodes):
        subtree = tree.subgraph(nodes)
        # nx.draw(subtree, with_labels=True)
        chains = []
        for node in nodes:
            if not tree.has_node(node):
                raise ValueError(f"{node} not in tree")
            if subtree.out_degree(node) == 0:
                chains.append(nx.shortest_path(tree, "tools", node)[1:])
        return chains

    df["chains"] = df["Taxonomy"].apply(find_chains)
    exploded = df.explode("chains").reset_index().rename(columns={"index": "tool_id"})
    exploded[["Taxonomy - 1st", "Taxonomy - 2nd", "Taxonomy - 3rd"]] = pd.DataFrame(exploded["chains"].tolist(), index=exploded.index)
    
    return exploded


def _process_urls(urls):
    homepage_url = None
    github_url = None
    for i, url in enumerate(str(urls).split(",")):
            url = url.strip()
            if i == 0:
                homepage_url = url
            if "//github.com/" in url:
                github_url = url
                continue
    return homepage_url, github_url


if __name__ == "__main__":
    transform()
