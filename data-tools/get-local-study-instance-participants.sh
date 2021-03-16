#!/bin/bash 

set -e

if [ $# -lt 2 ]
then
    echo "You need to provide a study-id and study-instance-id"
    exit 1
fi

STUDYID="$1"
INSTANCEID="$2"

# echo "curl -H'Authorization: 0d4113a3bb93554f9798355089747529a856ce63b33f271f6dccb836' http://127.0.0.1:5100/api/v1/studies/${STUDYID}/instances.json -o local-study-${STUDYID}-instances.json"

curl -H'Authorization: 0d4113a3bb93554f9798355089747529a856ce63b33f271f6dccb836' "http://127.0.0.1:5100/api/v1/studies/${STUDYID}/instances/${INSTANCEID}/participants.json" -o "local-study-${STUDYID}-instance-${INSTANCEID}-participants.json"

