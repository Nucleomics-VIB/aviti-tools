ENV_NAME := aviti_test_mask_webui

.PHONY: env env-update run test lint check-env clean

env:
	conda env create -f environment.yml

env-update:
	conda env update -f environment.yml --prune

check-env:
	@python -c "import sys, os; \
ok = os.environ.get('CONDA_DEFAULT_ENV') == '$(ENV_NAME)'; \
print('ENV OK' if ok else 'ERROR: activate $(ENV_NAME) first'); \
sys.exit(0 if ok else 1)"

run: check-env
	FLASK_APP=app.py FLASK_DEBUG=1 python -m flask run --host 127.0.0.1 --port 8765

test: check-env
	python -m pytest tests/ -v

lint: check-env
	python -m pyflakes . || true

clean:
	rm -rf __pycache__ */__pycache__ .pytest_cache
