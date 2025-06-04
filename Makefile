all:
	@echo "Expanding included files"
	cd getting-started/gsl-example && m4 -I. README.md.template > README.md
	cd pocs/stb_ds_example && m4 -I. README.md.template > README.md
	cd pocs/dhcp_example && m4 -I. README.md.template > README.md
