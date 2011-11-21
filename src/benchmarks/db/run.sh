#!/bin/sh

java -cp org-json.jar:postgresql-8.4-701.jdbc3.jar:. DataBlob $*
