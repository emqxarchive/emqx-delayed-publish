
EMQ X Delayed Publish
=====================

Delayed Publish Plugin. Publish to delayed topics:

    $delayed/1/topic
    $delayed/60/topic
    $delayed/3600/topic

Build the Plugin
----------------

make && make tests

Configuration
-------------

etc/plugins/emqx_delayed_publish.conf:

    ## publish filter topic
	delayed.publish.filter.1 = delay1/#
	delayed.publish.filter.2 = delay2/#

Load the Plugin
---------------

    ./bin/emqx_ctl plugins load emqx_delayed_publish

Example
--------

    mosquitto_sub -t "delay1" -q 1
    mosquitto_pub -t "$delayed/1/delay1" -q 1 -m 'hello'

License
-------

Apache License Version 2.0

Author
------

EMQ X-Men Team.

