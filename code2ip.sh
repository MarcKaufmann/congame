#/bin/bash

set -e

# Include these files in full
for f in README.md features-current-and-future.md congame-doc/LICENSE Dockerfile* $(find congame-identity -type f -name '*.rkt') $(ls congame-web-migrations/*.sql) $(ls congame-identity-migrations/*.sql) $(ls -p ci/* | grep -v /); 
do 
    echo "########## START OF FILE: ${f} ##########"
    echo " "
    cat ${f}
    echo " "
    echo "########## END OF FILE ${f}  ##########"
done

# Include only the header of these files
for f in $(find congame-{core,doc,example-study,pjb-studies,price-lists,smtp-proxy,tests,web} -type f -name '*.rkt') $(find resources/{css,js} -type f -regex '.*\(.css\|.scss\|.js\)') $(ls -p data-tools/*.{sh,R,Rproj} | grep -v /); 
do 
    echo "########## START OF FILE: ${f} (only first 30 lines included) ##########"
    echo " "
    cat ${f} | head -n 30
    echo " "
    echo "########## END OF FILE ${f}  ##########"
done
