#!/usr/bin/env python

import heatmap
import random
def random_heat():
    pts = []
    for x in range(400):
        pts.append((random.random(), random.random() ))

    print "Processing %d points..." % len(pts)

    hm = heatmap.Heatmap()
    img = hm.heatmap(pts)
    img.save("classic.png")

import os
def read_api_key():
    with open (os.environ['HOME']+"/script/GOOGLE_PLACES_API_KEY", "r") as myfile:
        return myfile.read().replace('\n', '')

from googleplaces import GooglePlaces, types, lang
def groceries():
    google_places = GooglePlaces(read_api_key())
    
    # You may prefer to use the text_search API, instead.
    query_result = google_places.radar_search(
            keyword='',
            lat_lng={'lat': 40.645244, 'lng': -73.9449975} ,# keyword='Fish and Chips',
            radius=20000, types=[types.TYPE_GROCERY_OR_SUPERMARKET])


    for place in query_result.places:
        #print place.get_details()
        #print place.name
        print place.geo_location

    print 'len: '+str(len(query_result.places))


if __name__ == "__main__":    
    groceries()
