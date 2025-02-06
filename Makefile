.PHONY: figures

VENV_NAME?=venv
PYTHON=${VENV_NAME}/bin/python

venv: $(VENV_NAME)/bin/activate

$(VENV_NAME)/bin/activate: requirements.txt
	test -d $(VENV_NAME) || python3.11 -m venv $(VENV_NAME)
	${PYTHON} -m pip install -r requirements.txt
	touch $(VENV_NAME)/bin/activate

code_anonymized.zip:
	mkdir -p code_anonymized
	cat public-files.txt | xargs -J % cp -r % code_anonymized/
	sed -i '' -E -e 's@(Ryan Steed|Open Source Audit Tooling|rbsteed)@ANONYMOUS@g' code_anonymized/*md
	sed -i '' -E -e 's@(Ryan Steed|Open Source Audit Tooling|rbsteed)@ANONYMOUS@g' code_anonymized/Makefile
	zip -r code_anonymized.zip code_anonymized
	rm -rf code_anonymized

figures:
	mkdir -p figures
	-rm figures/*
	while IFS= read -r fig; do \
		ln "$$PWD/plots/$$fig" figures/${fig##*/}; \
	done < figures.txt
