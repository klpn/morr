#!/bin/sh

while read f
do
    url="https://cdn.who.int/media/docs/default-source/world-health-data-platform/mortality-raw-data/${f}"
    curl -LO "${url}" && unzip "${f}" && rm "${f}"
done < whofiles

