#!/bin/bash

echo "testing";
envsubst '\$PORT' < /etc/nginx/nginx.template.conf > /etc/nginx/nginx.conf && exec nginx -g 'daemon off;';