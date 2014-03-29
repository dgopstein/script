#!/usr/bin/env python

import heatmap
import random
def map_heat(pts):
    print "Processing %d points..." % len(pts)

    hm = heatmap.Heatmap()
    img = hm.heatmap(pts)
    hm.saveKML("groceries.kml")

import os
def read_api_key():
    with open (os.environ['HOME']+"/script/GOOGLE_PLACES_API_KEY", "r") as myfile:
        return myfile.read().replace('\n', '')

from googleplaces import GooglePlaces, types, lang
def groceries():
    #BROOKLYN={'lat': 40.645244, 'lng': -73.9449975}
    BROOKLYN={'lat': 40.692, 'lng': -73.983}

    google_places = GooglePlaces(read_api_key())
    
    # You may prefer to use the text_search API, instead.
    query_result = google_places.radar_search(
            keyword='',
            lat_lng=BROOKLYN ,# keyword='Fish and Chips',
            radius=1850, types=[types.TYPE_GROCERY_OR_SUPERMARKET])


    pts = []
    for place in query_result.places:
        #print place.geo_location
        pts.append((place.geo_location['lng'], place.geo_location['lat']))

    return pts



if __name__ == "__main__":    
    map_heat(groceries())
