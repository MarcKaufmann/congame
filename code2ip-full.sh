#/bin/bash

set -e

# Include these files in full
for f in README.md congame-doc/LICENSE features-current-and-future.md Dockerfile* $(ls ci/*); #$(find congame-identity -type f -name '*.rkt') $(ls congame-web-migrations/*.sql) $(ls congame-identity-migrations/*.sql) $(ls -p ci/* | grep -v /);
do 
    echo "########## START OF FILE: ${f} ##########"
    echo " "
    cat ${f}
    echo " "
    echo "########## END OF FILE ${f}  ##########"
    echo ""
done

# Include only the header of these files
for f in $(find congame-{core,doc,example-study,identity,identity-migrations,pjb-studies,price-lists,smtp-proxy,tests,web,web-migrations} -type f \( -iname '*.rkt' -o -iname '*.sql' -o -iname '*.scrbl' \) ) $(ls data-tools/*.{sh,R,Rproj,rkt}) $(find omnitrack studies -type f \( -iname '*.rkt' -o -iname '*.scrbl' \) ) $(find resources/{css,js} -type f -regex '.*\(.css\|.scss\|.js\)');
do 
    echo "########## START OF FILE: ${f} (only part of file included) ##########"
    echo " "
    head -n 30 ${f}
    echo " "
    echo "... ### Rest of file omitted"
    echo "########## END OF FILE ${f}  ##########"
    echo ""
done
