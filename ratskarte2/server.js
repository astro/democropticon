var express = require('express');
var connect = require('connect');
var request = require('request');

var app = express();

app.get('/_search', function(req, res) {
    console.log("req", req.query);
    var n = Number(req.query.n);
    var s = Number(req.query.s);
    var w = Number(req.query.w);
    var e = Number(req.query.e);
    var zoom = Number(req.query.zoom);
    
    var filter = {
        and: [
            {
                geo_bounding_box: {
                    location: {
                        top_left: { lat: n, lon: w },
                        bottom_right: { lat: s, lon: e }
                    }
                }
            }
        ]
    };
    var search;
    if ((search = req.query.search)) {
        filter.and.push({
            query: {
                query_string: {
                    query: search
                }
            }
        });
    }
    var body = {
        size: 100,
        query: {
            filtered: {
                query: {
                    match_all: {}
                },
                filter: filter
            }
        },
        sort: [
            '_score',
            { _script: {
                script: "doc['locations.lat'].values.length",
                type: 'number',
                order: 'asc'
            } },
            { 'session.started_at': { order: 'desc' } },
            { file_name: { order: 'asc' } }
        ]
    };
    if (zoom) {
        body.aggregations = {
            zoomedView: {
                filter: filter,
                aggregations:{
                    zoom1:{
                        geohash_grid: {
                            field: 'location',
                            precision: zoom
                        }
                    }
                }
            }
        };
    }
    request.post("http://localhost:9200/ratsinfo/pdf/_search", {
        json: body
    }).pipe(res);
});

app.use(connect.compress());
app.use(express.static(__dirname + '/static'));
app.listen(parseInt(process.env.PORT || "8000", 10), '::');
