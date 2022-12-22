MANFOLDER=man
MANNAME=dokumentace.pdf
IMAGENAME=acc-clusters
VERSIONNAME=rc1

info:
	echo "To build the manual, say 'make man'"
	echo "To build the docker file, say 'make docker'"

man:	$(MANFOLDER)/$(MANNAME)
$(MANFOLDER)/$(MANNAME):
	(cd $(MANFOLDER) && make)

docker:	Dockerfile $(MANFOLDER)/$(MANNAME)
	docker build -t $(IMAGENAME):$(VERSIONNAME) .
