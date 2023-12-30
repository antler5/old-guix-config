# I kinda hate Makefiles.
OUT = ./artifacts
SHELL = guix time-machine --channels=../channels-default.scm --commit=cacc0cb6ab22218a3783a51ba9986405ede4e0d8 -- shell -m manifest.scm -- bash

VARS = PK KEK db dbx
BACKUPS = $(foreach var,$(VARS),$(OUT)/old_$(var).esl)
CERTS   = $(foreach var,$(filter-out dbx,$(VARS)),$(OUT)/$(var).crt)

.PHONY: build install clean
build: $(BACKUPS) $(CERTS:crt=esl) $(CERTS:crt=auth) $(OUT)/old_dbx.auth

.PRECIOUS: $(CERTS)

$(OUT)/old_%.esl:
	@mkdir -p $(OUT)
	efi-readvar -v $* -o $@

$(OUT)/%.crt:
	@mkdir -p $(OUT)
	openssl req -new -x509 -newkey rsa:2048 -subj "/CN=$* key/" -keyout $(OUT)/$*.key -out $(OUT)/$*.crt -days 3650 -nodes -sha256

$(OUT)/%.esl: $(OUT)/%.crt $(OUT)/$(if $(filter PK KEK,%),PK,KEK).crt
	cert-to-efi-sig-list -g "$$(uuidgen)" $< $@

%.auth: %.esl
	KEY=$(if $(filter PK KEK,$(basename $(notdir $@))),PK,KEK) && \
	  sign-efi-sig-list -k $(OUT)/$$KEY.key -c $(OUT)/$$KEY.crt $$KEY $< $@

install: build
	chattr -i /sys/firmware/efi/efivars/*
	efi-updatevar -e -f $(OUT)/KEK.esl KEK
	efi-updatevar -e -f $(OUT)/db.esl db
	efi-updatevar -e -f $(OUT)/old_dbx.esl dbx
	efi-updatevar -f $(OUT)/PK.auth PK

clean:
	find ./artifacts -type f ! -iname old_\* -exec rm -f \{} +
