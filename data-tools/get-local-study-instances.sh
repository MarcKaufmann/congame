#!/bin/bash 

set -e

if [ $# -eq 0 ]
then
    echo "No study-id provided, you need to provide one"
    exit 1
fi

STUDYID="$1"

# echo "curl -H'Authorization: 0d4113a3bb93554f9798355089747529a856ce63b33f271f6dccb836' http://127.0.0.1:5100/api/v1/studies/${STUDYID}/instances.json -o local-study-${STUDYID}-instances.json"

curl -H'Authorization: 0d4113a3bb93554f9798355089747529a856ce63b33f271f6dccb836' "http://127.0.0.1:5100/api/v1/studies/${STUDYID}/instances.json" -o "local-study-${STUDYID}-instances.json"

