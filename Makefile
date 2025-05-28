all:
	@echo "Expanding included files"
	cd getting-started/gsl-example && m4 -I. README.md.template > README.md
