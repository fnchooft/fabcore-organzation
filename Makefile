ROOT_DIR := $(dir $(realpath $(lastword $(MAKEFILE_LIST))))

all:
	@echo "Expanding included files"
	@echo "ROOTDIR: $(ROOT_DIR)"
	cd getting-started/gitbook-ds && m4 -I. -I$(ROOT_DIR) README.md.template > README.md
	cd getting-started/gsl-example && m4 -I. README.md.template > README.md
	cd pocs/stb_ds_example && m4 -I. README.md.template > README.md
	cd pocs/dhcp_example && m4 -I. README.md.template > README.md
