# Kie Workbench Client

Client for interaction with KIE workbench [rest controller](https://github.com/kiegroup/appformer/tree/master/uberfire-rest),
 based on API version 2.4.0.

## Interactive usage from GHCi

```Haskell
:set -XOverloadedStrings
:set -XRecordWildCards
env <- initClientEnv (BaseUrl Http "localhost" 8080 "business-central/rest")
auth = BasicAuthData "testadmin" "admin1234;"
WorkbenchClient{..} = mkWorkbenchClient auth
runClientM getSpaces env
```
