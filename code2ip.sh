#/bin/bash

set -e

for f in README.md congame-doc/LICENSE $(find congame-{core,doc,example-study,price-lists} -type f -name '*.rkt') $(ls congame/*.rkt); 
do 
    echo "###################### START OF FILE ####################"
    echo "FILENAME: ${f}"
    echo " "
    cat ${f}
    echo " "
    echo "###################### END OF FILE ####################"
done
