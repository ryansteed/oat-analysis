library(pacman)

pacman::p_load(
  ggplot2,
  tidyr,
  dplyr,
  janitor,
  forcats,
  stringr,
  scales,
  treemap,
  stargazer,
  ggrepel,
  shades,
  ggExtra
)
pacman::p_install_gh("coolbutuseless/ggpattern")

# pattern_scale = ggpattern::scale_pattern_type_discrete()
labels = list(
  "license"="License",
  "internal_external"="Practitioner Type",
  "org_type_creator"="Creator",
  "audit_target_type"="Audit Target",
  "has_code_or_data"="Format",
  "has_paper_or_handbook"="Format",
  "usage_type" = "Type"
)
var_labels = list(
  "cb_employee_count" = "# Employees",
  "cb_estimated_revenue_range" = "Estimated Revenue",
  "cb_total_funding_usd_mil" = "Total funding (millions USD)",
  "gh_forks" = "Github forks",
  "gh_issues" = "Github issues",
  "gh_stars" = "Github stars",
  "usage" = "Usage",
  "cites" = "Citations"
)
factors = c(
  "audit_target_type", "license", "internal_external", "org_type_creator", "taxonomy_1st", 
  "taxonomy_2nd", "taxonomy_3rd", "cb_employee_count", "cb_estimated_revenue_range", "cb_ipo_status",
  "cb_operating_status", "cb_funding_status", "format_todo"
)

cb_numerics = c(
  "cb_number_of_funding_rounds", "cb_total_funding_usd_mil", "cb_total_equity_funding_amount_currency_in_usd",
  "cb_price_currency_in_usd", "cb_se_mrush_average_visits_6_months", "cb_se_mrush_global_traffic_rank",
  "cb_apptopia_number_of_apps", "cb_apptopia_downloads_last_30_days",
  "cb_g2_stack_total_products_active", "cb_i_pqwery_patents_granted",
  "cb_aberdeen_it_spend_currency_in_usd", "cb_money_raised_at_ipo_currency_in_usd"
)
cb_cats = c("cb_employee_count", "cb_estimated_revenue_range", "cb_operating_status", "cb_ipo_status", "cb_funding_status")

clean_airtable = function(airtable) {
  airtable %>%
    rename(
      cb_name = cb_organization_name,
      cb_employee_count = cb_number_of_employees,
      cb_last_funding_on = cb_last_funding_date
    ) %>%
    mutate(taxonomy_2nd = replace_na(taxonomy_2nd, "Other")) %>%
    mutate_at(factors, as.factor) %>%
    separate(cb_employee_count, c("cb_employee_count_min", "cb_employee_count_max"), sep="-", remove=F) %>%
    mutate_at(c("cb_employee_count_min", "cb_employee_count_max"), as.numeric) %>%
    mutate(
      audit_target_type = fct_collapse(audit_target_type, Any = c("ADS,Online Platform", "Online Platform,ADS", "Any", "Online Platform,Large Pre-trained Models"), Other = "other"),
      internal_external = fct_recode(internal_external, Both = "Internal,External"),
      license = fct_recode(license, "Open Source"="OpenSource"),
      org_type_creator = fct_recode(org_type_creator, "Non-Profit"="Non Profit", "Gov't"="Govt"),
      cb_employee_count = fct_reorder(cb_employee_count, cb_employee_count_min),
      cb_estimated_revenue_range = fct_recode(cb_estimated_revenue_range, "<$1M"="Less than $1M"),
      cb_estimated_revenue_range = fct_relevel(cb_estimated_revenue_range, "<$1M", "$1M to $10M", "$10M to $50M", "$50M to $100M", "$100M to $500M", "$500M to $1B", "$1B to $10B", "$10B+"),
      taxonomy_1st = recode_factor(
        taxonomy_1st,
       "Harms Discovery"="Harms Disc.",
       "Standards Identification & Management"="Standards",
       "Transparency Infrastructure"="Transp. Infra.",
       "Data Collection"="Data Coll.",
       "Performance Analysis"="Perf. Analysis",
       "Audit Communication"="Audit Comm.",
       "Advocacy"="Advocacy"
      ),
      taxonomy_2nd = recode_factor(
        taxonomy_2nd,
        "Participatory Standard-Setting"="Participatory",
        "Regulatory Awareness"="Reg. Awareness",
        "Secure and Privacy-Preserving Sharing"="Secure/Private Sharing",
        "Qualitative Analysis"="Qual. Analysis",
        "Tools for Resistance"="Resistance",
        "Dataset Visualization"="Dataset Viz.",
        "Structured Access / API Access" = "Structured Access",
        "Organizing Resistance" = "Org. Resistance"
      ),
      taxonomy_1st_condensed = fct_collapse(taxonomy_1st, "Comm./Advoc."=c("Audit Comm.", "Advocacy")),
      taxonomy_accountability = fct_collapse(taxonomy_1st, "Accountability"=c("Harms Disc.", "Audit Comm.", "Advocacy"), "Evaluation"=c("Standards", "Transp. Infra.", "Data Coll.", "Perf. Analysis")),
      cb_last_funding_on = as.Date(cb_last_funding_on),
      has_code_or_data = as.factor(if_else(grepl("API|Software|Repository", format_todo), "Has code/data", "No code/data")),
      has_paper_or_handbook = as.factor(if_else(grepl("Handbook|White paper", format_todo), "Has paper/handbook", "No paper/handbook"))
    ) %>%
    select(
      -one_of(
        "cb_website", "cb_twitter", "cb_facebook", "cb_linked_in", "cb_contact_email", "cb_phone_number", "cb_hub_tags", "cb_full_description", "cb_number_of_sub_orgs",
        "cb_top_5_investors", "cb_school_method", "cb_acquisition_terms", "cb_stock_symbol_url", "cb_similar_companies", "cb_accelerator_program_type"
      ),
      -starts_with("cb_cb_rank"), -starts_with("cb_trend"), -contains("headquarters"), -ends_with("_precision"), -ends_with("_currency"), -ends_with("amount"), -starts_with("cb_last_funding_amount"),
      -starts_with("cb_last_equity_funding_amount"), -starts_with("cb_transaction"), -contains("contact"), -contains("location"), -contains("investments"), -contains("accelerator"), -contains("person_name")
    )
}

save = function(name, width, height) {
  ggsave(sprintf("%s.pdf", name), width=width, height=height)
  ggsave(sprintf("%s.png", name), width=width, height=height)
}

count_tools = function(field, grouping) {
  if ("taxonomy_1st_condensed" %in% grouping) {
    grouped = cleaned %>%
      group_by(!!sym(grouping), !!sym(field))
  }
  else if ("taxonomy_2nd" %in% grouping) {
    grouped = cleaned %>%
      group_by(taxonomy_1st_condensed, !!sym(grouping), !!sym(field))
  }
  else {
    grouped = cleaned %>%
      group_by(!!sym(grouping), !!sym(field))
  }
  grouped = grouped %>%
    summarise(count = n_distinct(name, organization), .groups="drop_last") %>%
    group_by(!!sym(grouping)) %>%
    mutate(
      total = sum(count),
      prop = count / total
    ) %>%
    drop_na()
  return(grouped)
}

bar_count = function(field, position, taxonomy_level) {
  agg = count_tools(field, taxonomy_level)
  print(agg)
  bar(agg, field, position, taxonomy_level) +
    ylab(if (position == "fill") "% of tools" else "# tools")
}

bar = function(agg, field, position, taxonomy_level) {
  bar_pattern(agg, field, position, taxonomy_level)
  # bar_dots(agg, field, position, taxonomy_level)
}

bar_pattern = function(agg, field, position, taxonomy_level) {
  ggplot(agg, aes(
    x=if (position == "fill") !!sym(taxonomy_level) else reorder(!!sym(taxonomy_level), total),
    y=count,
    fill=as.factor(!!sym(field)),
    colour=as.factor(!!sym(field)),
    pattern=as.factor(!!sym(field)),
    pattern_fill=as.factor(!!sym(field)),
    pattern_colour=as.factor(!!sym(field)),
    pattern_angle=as.factor(!!sym(field))
    # pattern_spacing=as.factor(!!sym(field))
  )) +
    ggpattern::geom_bar_pattern(position=position, stat="identity", pattern_density=0.15, pattern_spacing=0.05, pattern_key_scale_factor=0.5) +
    scale_x_discrete(limits=rev, labels=wrap_format(20)) +
    pattern_theme(field) +
    coord_flip() +
    xlab("Taxonomy Category") +
    guides(
      fill=guide_legend(
        nrow=2
      ),
      color="none"
    ) +
    theme(
      legend.position="top"
    )
}
bar_dots = function(agg, field, position, taxonomy_level) {
  ggplot(cleaned, aes(
    x=!!sym(taxonomy_level),
    # y=count,
    fill=as.factor(!!sym(field)),
    colour=!!sym(field)
  )) +
    geom_dotplot(method="histodot", stackgroups=T) +
    # ggpattern::geom_bar_pattern(position=position, stat="identity", pattern_density=0.15, pattern_spacing=0.05, pattern_key_scale_factor=0.5) +
    scale_x_discrete(limits=rev, labels=wrap_format(20)) +
    coord_flip() +
    xlab("Taxonomy Category") +
    guides(
      fill=guide_legend(
        nrow=2
      ),
      color="none"
    ) +
    theme(
      legend.position="top"
    )
}

pivot = function(grouped) {
  grouped %>% 
    summarise_all(list(N = ~ n(), Min = function(x) {if (length(na.omit(x)) > 0) min(x, na.rm=TRUE) else Inf}, Mean = ~ mean(.x, na.rm=TRUE), Median = ~ median(.x, na.rm=TRUE), Max = function(x) {if (length(na.omit(x)) > 0) max(x, na.rm=TRUE) else Inf}, SD = ~ sd(.x, na.rm=TRUE))) %>%
    ungroup()
}

agg_funds = function(df, field, grouping, agg_func, agg_col) {
  grouped = df %>%
    distinct(taxonomy_1st, !!sym(field), !!sym(grouping), !!sym(agg_col)) %>%
    group_by(!!sym(grouping)) %>%
    mutate(total = agg_func(!!sym(agg_col)))
  
  if (grouping == "taxonomy_1st") {
    grouped = grouped %>%
      group_by(!!sym(grouping), !!sym(field))
  }
  else {
    grouped = grouped %>%
      group_by(taxonomy_1st, !!sym(grouping), !!sym(field))
  }
  
  grouped = grouped %>%
    summarise(
      count = agg_func(!!sym(agg_col)),
      n = n(),
      total = first(total),
      .groups="drop_last"
    ) %>%
    drop_na()
  return(grouped)
}

palette = "Set2"
pattern_theme = function(field) {
  list(
    ggpattern::scale_pattern_discrete(
      name=labels[[field]],
      choices=c('stripe', 'circle', 'crosshatch')
    ),
    # ggpattern::scale_pattern_type_manual(name=labels[[field]], values=c(NA, NA, 'hexagonal', 'herringbone')) +
    # ggpattern::scale_pattern_spacing_discrete(name=labels[[field]], range=c(0.03, 0.05)) +
    shades::lightness(ggpattern::scale_pattern_fill_brewer(name=labels[[field]], palette=palette), scalefac(0.8)),
    shades::lightness(ggpattern::scale_pattern_colour_brewer(name=labels[[field]], palette=palette), scalefac(0.8)),
    ggpattern::scale_pattern_angle_discrete(name=labels[[field]], range=c(5, 85)),
    scale_colour_brewer(name=labels[[field]], palette=palette),
    shades::lightness(scale_fill_brewer(name=labels[[field]], palette=palette), scalefac(1.1)),
    theme_bw()
  )
}

bar_agg = function(df, taxonomy_level, field, position, agg_kind, agg_col) {
  if (agg_kind == "median") {
    agg_func = function(x) median(x, na.rm=T)
  }
  else if (agg_kind == "mean") {
    agg_func = function(x) mean(x, na.rm=T)
  }
  else if (agg_kind == "sum") {
    agg_func = function(x) sum(x, na.rm=T)
  }
  print(df)
  notna = df %>% drop_na(!!sym(agg_col))
  agg = agg_funds(notna, field, taxonomy_level, agg_func, agg_col)
  print(agg)
  bar = ggplot(
    agg,
    aes(
      x=!!sym(taxonomy_level), y=count,
      fill=as.factor(!!sym(field)),
      colour=!!sym(field),
      pattern=as.factor(!!sym(field)),
      pattern_fill=as.factor(!!sym(field)),
      pattern_colour=as.factor(!!sym(field)),
      pattern_angle=as.factor(!!sym(field))
    )
  ) +
    ggpattern::geom_bar_pattern(stat="identity", position=position, pattern_density=0.15, pattern_spacing=0.05, pattern_key_scale_factor=0.5) +
    scale_x_discrete(limits=rev, labels=wrap_format(20)) +
    pattern_theme(field) +
    xlab(paste0("Taxonomy Category (N=", nrow(notna), ")")) +
    coord_flip() +
    guides(
      fill=guide_legend(
        title=labels[[field]],
        nrow=2
      )
    ) +
    theme(
      legend.position="top"
    )
  bar
}

violin = function(df, col, field, taxonomy_level, label_col, logscale) {
  if (missing(logscale)) {
    logscale = F
  }
  fill = "All"
  n_label_thresh = 2
  points = df %>%
    drop_na(!!sym(col)) %>%
    distinct(!!sym(col), !!sym(field), !!sym(taxonomy_level), !!sym(label_col))  %>%
    group_by(!!(sym(taxonomy_level)), !!sym(field)) %>%
    # arrange(!!sym(field)) %>%
    mutate(
      q1 = quantile(!!sym(col), 0.25),
      q3 = quantile(!!sym(col), 0.75),
      iqr = q3 - q1,
      median = median(!!sym(col)),
      min = min(!!sym(col)),
      max = max(!!sym(col)),
      mean = mean(!!sym(col)),
      n = n(),
      sum = sum(!!sym(col)),
      is_outlier = !!sym(col) > q3 + iqr,
      any_outlier = max(is_outlier),
      label = ifelse(!!sym(col) >= nth(!!sym(col), n_label_thresh) | n <= n_label_thresh, str_trunc(!!sym(label_col), 10, ellipsis="."), NA),
      label_out = ifelse(is_outlier, label, NA),
      label_outliers = ifelse(any_outlier, str_trunc(paste("↑", paste0(label_out[!is.na(label_out)], collapse=", ")), 20), NA)
    ) %>%
    ungroup() %>%
    group_by(!!sym(taxonomy_level)) %>%
    mutate(
      panel_upper = max(q3 + 1.5 * iqr)
    ) %>%
    ungroup() %>%
    arrange(!!sym(field))
  points_display = points %>%
    filter(!is_outlier | logscale)
  stats = points %>%
    group_by(!!(sym(taxonomy_level)), !!sym(field)) %>%
    summarise_all(
      first
    )
  outliers = points %>%
    filter(is_outlier)
  print(outliers %>% select(!!sym(label_col), !!sym(label_col), !!sym(taxonomy_level), !!sym(field), !!sym(col)))
  print(stats %>% select(!!sym(taxonomy_level), !!sym(field), n, min, q1, median, mean, max, q3, sum))
  plt = ggplot(points_display, aes(x=!!sym(field), y=!!sym(col), fill=!!sym(field))) +
    geom_boxplot(
      data=stats %>% filter(n > 10),
      stat="identity",
      aes(x=!!sym(field), y=!!sym(col), fill=!!sym(field), lower=q1, upper=q3, middle=median, ymin=pmax(q1-1.5*iqr, min), ymax=pmin(q3+1.5*iqr, max)),
      # width=0.1
    ) +
    # for the points alone in their group, just plot them
    geom_point(data=points_display %>% filter(n <= 1), pch=21, na.rm=T, size=2, position=position_stack()) +
    # for others, use a dotplot
    geom_dotplot(binaxis='y', stackdir='center', abs_size=1.5, pch=21, drop=T, dotsize=1) +
    # ggbeeswarm::geom_beeswarm(groupOnX=F, cex=2, size=1,  pch=21) +
    ggrepel::geom_text_repel(
      aes(y=!!sym(col), label=label), na.rm=T, size=2.5, show.legend=F,
      max.overlaps = 4
      # hjust="left", vjust="bottom", nudge_x=0.05, nudge_y=0.05, check_overlap=T
    )
  if (logscale) {
    plt = plt + scale_y_continuous(trans=scales::pseudo_log_trans(base = 10))
  }
  else {
    plt = plt + geom_label(data=stats, aes(x=!!sym(field), y=panel_upper*1.1, label=label_outliers), na.rm=T, size=0.5, label.size=5, check_overlap=T, alpha=0.5, show.legend=F)
  }
  palette = "Set2"
  print(levels(df[[field]]))
  palette_labels = levels(df[[field]])
  names(palette_labels) = levels(df[[field]])
  palette_values = brewer_pal(palette=palette)(length(levels(df[[field]])))
  names(palette_values) = levels(df[[field]])
  plt = plt +
    scale_fill_manual(labels=palette_labels, values=palette_values) +
    xlab("Taxonomy Category") +
    # xlab(paste0("Taxonomy Category (N=", nrow(points), ")")) +
    guides(
      fill=guide_legend(
        title=labels[[field]],
        nrow=2
      ),
      text="none"
    ) +
    theme_bw() +
    theme(
      legend.position = "top",
      axis.text.x = element_text(angle=15, vjust=1, hjust=1),
      axis.text.y = element_text(size=7),
      panel.spacing = unit(0.5, "lines")
    ) +
    ylab(var_labels[col])
  if (stats %>% select(!!sym(taxonomy_level)) %>% n_distinct() > 4) {
    plt = plt + facet_wrap(vars(!!sym(taxonomy_level)), scales="free")
    save(sprintf("plots/boxes/%s_%s", col, field), 6.5, 6.5)
  } else {
   plt = plt + facet_grid(cols=vars(!!sym(taxonomy_level)), scales="free", space="free_x") 
   save(sprintf("plots/boxes/%s_%s", col, field), 6.5, 4)
  }
  plt
}

hist_plot = function(df, col, field, is_cat) {
  df = df %>%
    distinct(taxonomy_1st, taxonomy_1st_condensed, name, organization, !!sym(col), !!sym(field)) %>%
    drop_na(!!sym(col))
  if (missing(is_cat)) {
    is_cat = T
  }
  print(nrow(df))
  plt = ggplot(df, aes(x=!!sym(col), fill=as.factor(!!sym(field))))
  if (is_cat) {
    plt = plt +
      ggpattern::geom_bar_pattern(aes(
        fill=as.factor(!!sym(field)),
        colour=as.factor(!!sym(field)),
        pattern=as.factor(!!sym(field)),
        pattern_fill=as.factor(!!sym(field)),
        pattern_colour=as.factor(!!sym(field)),
        pattern_angle=as.factor(!!sym(field))
      ), pattern_density=0.15, pattern_spacing=0.05, pattern_key_scale_factor=0.5) +
      pattern_theme(field)
  }
  else {
    plt = plt +
      ggpattern::geom_histogram_pattern(bins=7, pattern_density=0.15, pattern_spacing=0.05, pattern_key_scale_factor=0.5, aes(
        fill=as.factor(!!sym(field)),
        colour=as.factor(!!sym(field)),
        pattern=as.factor(!!sym(field)),
        pattern_fill=as.factor(!!sym(field)),
        pattern_colour=as.factor(!!sym(field)),
        pattern_angle=as.factor(!!sym(field))
      )) +
      pattern_theme(field)
  }
  plt = plt +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
    ylab("# Tools") +
    guides(
      fill=guide_legend(
        title=labels[[field]],
        nrow=2
      )
    ) +
    xlab(var_labels[col]) +
    theme(
      legend.position="top"
    ) + 
    # guides(fill = "none") +
    facet_wrap(~ taxonomy_1st_condensed, ncol=3, scales=if (is_cat) "free_y" else "free")
  save(sprintf("plots/hists/%s_%s", col, field), 6, 6)
  plt
}