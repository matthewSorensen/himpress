all: css/style.min.css js/impress.min.js

css/style.min.css: css/reset.sass css/impress.scss css/haskell.scss css/style.scss
	sass --scss -t compressed css/style.scss $@

js/impress.min.js: js/impress.js
	uglifyjs $^ > $@
