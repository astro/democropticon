var map = L.map($('#map')[0]);
map.setView([51.0474, 13.7464], 10);
map.addLayer(
    L.tileLayer("http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", {
	attribution: "OpenStreetMap.org",
	maxZoom: 17
    })
);

var markers = L.layerGroup().addTo(map);

var ZOOM_TABLE = {
    10: 5,
    11: 5,
    12: 6,
    13: 6,
    14: 6,
    15: 7,
    16: 8,
    17: 10
};
function zoomToPrecision(mapZoom) {
    return ZOOM_TABLE[Math.max(10, Math.min(17, mapZoom))];
}

function displayHits(hits) {
    $('#docList').empty();
    hits.forEach(function(hit) {
        var doc = hit._source;
        var li = $("<li></li>");
        if (doc.session) {
            var date = new Date(doc.session.started_at);
            li.append($("<p class='date'></p>").text(
                date.getDate() + "." +
                    (date.getMonth() + 1) + "." +
                    date.getFullYear()
            ));
            li.append($("<h2></h2>").text(doc.session.committee));
        }
        if (doc.context) {
            doc.context.forEach(function(c) {
                if (c.decision) {
                    var p = $("<p class='decision'></p>");
                    if (/^Zustimmung/i.test(c.decision) ||
                        /^erledigt/i.test(c.decision) ||
                        /^zur Kenntnis genommen/i.test(c.decision)) {

                        p.addClass('accepted');
                    } else if (/^Ablehn/i.test(c.decision)) {
                        p.addClass('rejected');
                    }
                    p.text(c.decision);
                    li.append(p);
                }
                li.append($("<h3></h3>").text(c.description));
            });
        }
        var id = hit._id.split("/")[1];
        var description = doc.description || doc.file_name;
        li.append(
            $("<p></p>").append(
                $("<a type='application/pdf'></a>")
                    .text(description)
                    .attr('href', "http://ratsinfo.dresden.de/getfile.php?id=" + id + "&type=do")
            ));
        li.on('mouseenter', function() {
            if (doc.locations) {
                doc.locations.forEach(function(loc) {
                    markers.eachLayer(function(marker) {
                        var geo = marker.options.geohash;
                        if (loc.lon >= geo.longitude[0] &&
                            loc.lon < geo.longitude[1] &&
                            loc.lat >= geo.latitude[0] &&
                            loc.lat < geo.latitude[1]) {

                            $(marker._icon).addClass('indicated');
                        }
                    });
                });
            }
        });
        li.on('mouseleave', function() {
            markers.eachLayer(function(marker) {
                $(marker._icon).removeClass('indicated');
            });
        });
        $('#docList').append(li);
    });
}

function updateSelection() {
    var bounds = map.getBounds();
    var n = bounds.getNorth();
    var s = bounds.getSouth();
    var w = bounds.getWest();
    var e = bounds.getEast();
    console.log("zoom", map.getZoom(), "precision", zoomToPrecision(map.getZoom()));
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
    if ((search = $('#search input').val())) {
        filter.and.push({
            query: {
                query_string: {
                    query: search
                }
            }
        });
    }
    callSearch({
        size: 30,
        query: {
            filtered: {
                query: {
                    match_all: {}
                },
                filter: filter
            }
        },
        sort: [
            { 'session.started_at': { order: 'desc' } },
            { file_name: { order: 'asc' } }
        ],
        aggregations: {
            zoomedView: {
                filter: filter,
                aggregations:{
                    zoom1:{
                        geohash_grid: {
                            field: 'location',
                            precision: zoomToPrecision(map.getZoom())
                        }
                    }
                }
            }
        }
    }, function(error, result) {
        if (error) {
            $('#status').text("Fehler :-(");
        }
        if (result && result.aggregations && result.aggregations.zoomedView) {
            markers.clearLayers();

            var zoomed = result.aggregations.zoomedView;
            console.log(zoomed.zoom1.buckets.length + " buckets for " + zoomed.doc_count + " hits");
            zoomed.zoom1.buckets.forEach(function(bucket) {
                var caption = bucket.doc_count + "";
                var size = Math.ceil(10 + Math.min(24, Math.sqrt(bucket.doc_count)));
                var icon = L.divIcon({
                    className: 'mapIcon',
                    html: caption,
                    iconSize: size
                });
                var geo = GeoHash.decodeGeoHash(bucket.key);
                var loc = { lon: geo.longitude[2], lat: geo.latitude[2] };
                var marker = L.marker(loc, {
                    title: bucket.doc_count + " Dokumente", 
                    geohash: geo,
                    icon: icon
                });
                marker
                    .addTo(markers)
                    .on('click', onClickMarker);
                var iconEl = $(marker._icon);
                iconEl.css('width', size + "px");
                iconEl.css('height', size + "px");
                iconEl.css('line-height', size + "px");
            });
        }
        if (result && result.hits && result.hits.hits) {
            var hits = result.hits.hits;
            console.log(hits.length + "/" + result.hits.total + " hits");
            $('#status').text(result.hits.total + " Dokumente");

            displayHits(hits);
        }
    });

    $('#status').text("Laden…");
}

map.on('moveend', updateSelection);
$('#search input')
    .on('change', updateSelection)
    .on('keypress', function(ev) {
        if (ev.which == 13) {
            updateSelection();
        }
    });
updateSelection();

function onClickMarker(ev) {
    markers.eachLayer(function(marker) {
        $(marker._icon).removeClass('selected');
    });
    $(ev.target._icon).addClass('selected');

    var geo = ev.target.options.geohash;
    var s = geo.latitude[0];
    var n = geo.latitude[1];
    var w = geo.longitude[0];
    var e = geo.longitude[1];

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
    if ((search = $('#search input').val())) {
        filter.and.push({
            query: {
                query_string: {
                    query: search
                }
            }
        });
    }
    callSearch({
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
            { 'session.started_at': { order: 'desc' } },
            { file_name: { order: 'asc' } }
        ]
    }, function(error, result) {
        if (error) {
            $('#status').text("Fehler :-(");
        }
        if (result && result.hits && result.hits.hits) {
            var hits = result.hits.hits;
            console.log(hits.length + "/" + result.hits.total + " hits");
            $('#status').text(result.hits.total + " Dokumente");

            displayHits(hits);
        }
    });

    $('#status').text("Laden…");
}


function callSearch(data, cb) {
    $.ajax({
        url: "http://localhost:9200/ratsinfo/pdf/_search",
        method: 'POST',
        data: JSON.stringify(data),
        contentType: 'application/json',
        processData: false,
        success: function(result) {
            console.log("callSearch result:", result);
            cb(null, result);
        },
        error: function(error) {
            console.error("error", arguments);
            cb(error);
        }
    });
}
