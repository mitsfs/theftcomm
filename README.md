# theftcomm
Theftcomm Validation and TIF Generation Code

The docker container builds a static binary. You can also compile locally with stack
You will need to download https://github.com/pweaver/iCalendar (develop branch) to iCalendar 

## Building

    `docker build . -t theftcomm`
    
    This builds and tests the app. Incremental builds should be pretty fast assuming stack.yaml and theftcomm.cabal are unchanged.
    
## Running
    
    `docker run --rm -it theftcomm`
    
## Copy Binary
    `docker run --rm -v $(pwd):/out theftcomm cp /usr/local/bin/theftcomm /out `
