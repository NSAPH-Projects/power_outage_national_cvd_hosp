# This script sets up the cluster for data cleaning

# set proxies for GitHub access
Sys.setenv(http_proxy="http://rcproxy.rc.fas.harvard.edu:3128")
Sys.setenv(https_proxy="http://rcproxy.rc.fas.harvard.edu:3128")

# add token
gitcreds::gitcreds_set()
