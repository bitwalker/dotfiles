#!/env/bash

function aping {
    # Default variable values
    target=localhost
    voice=Victoria
    # Parse arguments
    if [ $# -eq 0 ]; then
        echo 'You must provide at least one argument. First argument is host, second is the voice to use.'
        return
    elif [ $# -eq 1 ]; then
        target=$1
    else
        target=$1
        voice=$2
    fi

    # Loop until the host is no longer found, or execution is cancelled
    while [ true ]
    do
        # Store the result of pinging the target
        time=`ping -c 1 $target 2>/dev/null`
        # If the exit value of ping is greater than 0, it failed to reach the host, or errored out
        if [ $? -gt 0 ]
        then
            echo 'Host was not found'
            break
        else
            # Filter the output of ping through grep to retrieve the line containing the response time
            # capture the time value using sed, and send it to say using xargs
            echo $time | grep time | sed -E 's/.*time=([[:digit:]]+)\.[[:digit:]]+.*/Response in \1 milliseconds/' | xargs say -v $voice
            sleep 1
        fi

    done
}
