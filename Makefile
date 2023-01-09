MANFOLDER=man
MANNAME=dokumentace.pdf
OWNER=mkvasnicka/
IMAGENAME=acc-clusters
VERSIONNAME=latest
DOCKERDEPS = $(wildcard *.R) update_data.sh renv.lock $(wildcard config/*)

info:
	@echo "To build the manual, say 'make man'"
	@echo "To build the docker file, say 'make docker'"

man:	$(MANFOLDER)/$(MANNAME)
$(MANFOLDER)/$(MANNAME):	$(MANFOLDER)/$(MANNAME:pdf=Rmd)
	(cd $(MANFOLDER) && make)

docker:	Dockerfile $(DOCKERDEPS) $(MANFOLDER)/$(MANNAME)
	docker build -t $(OWNER)$(IMAGENAME):$(VERSIONNAME) .

clean:
	$(MAKE) -C $(MANFOLDER) clean
