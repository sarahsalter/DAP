#!/usr/bin/env bash

rsync -avz --delete --delete-excluded --exclude **/text-versions/ \
		govtrack.us::govtrackdata/congress/113/bills .
