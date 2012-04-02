all: css/style.css js/impress.min.js

css/style.css: css/reset.css css/impress.css css/haskell.css
	cat $^ > $@

js/impress.min.js: js/impress.js
	uglifyjs $^ > $@
