"use strict";

if (!window.console)
    window.console = {
	log: function() { },
	warn: function() { },
	error: function() { }
    };

var app = angular.module('app', []);

app.directive('leaflet', function() {
    return {
	restrict: 'A',
	link: function($scope, element, attrs) {
	    var map = L.map(element[0]);
	    map.setView([51.0474, 13.7464], 10);
	    map.addLayer(
		// L.tileLayer('http://{s}.tile.cloudmade.com/API-key/997/256/{z}/{x}/{y}.png', {
		//     attribution: 'Map data &copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors, <a href="http://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>, Imagery © <a href="http://cloudmade.com">CloudMade</a>',
		//     maxZoom: 18
		// })
		L.tileLayer("http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", {
		    attribution: "OpenStreetMap.org",
		    maxZoom: 18
		})
	    );
	    map.on('click', function(e) {
		var lat = e.latlng.lat,
		    lon = e.latlng.lng;
		$scope.onClickMap(lon, lat);
	    });
	    var markers = [];
	    $scope.setMarkers = function(coords) {
		markers.forEach(function(marker) {
		    map.removeLayer(marker);
		});
		markers = [];

		var coordsCounts = {};
		coords.map(function(coord) {
		    var key = coord.lon + ";" + coord.lat;
		    if (!coordsCounts.hasOwnProperty(key))
			coordsCounts[key] = 0;
		    coordsCounts[key]++;
		});
		for(var key in coordsCounts)
		    if (coordsCounts.hasOwnProperty(key)) {
			var lonlat = key.split(";");
			markers.push(L.marker({
			    lon: parseFloat(lonlat[0]),
			    lat: parseFloat(lonlat[1])
			}, {
			    clickable: false,
			    keyboard: false,
			    title: (coordsCounts[key] <= 1 ?
				    "Ein Dokument" :
				    coordsCounts[key] + " Dokumente")
			}).addTo(map));
		    }
		var minLat, maxLat, minLon, maxLon;
		coords.forEach(function(coord) {
		    if (typeof minLat === 'undefined' ||
		        minLat < coord.lat)
			minLat = coord.lat;
		    if (typeof maxLat === 'undefined' ||
		        maxLat > coord.lat)
			maxLat = coord.lat;
		    if (typeof minLon === 'undefined' ||
		        minLon < coord.lon)
			minLon = coord.lon;
		    if (typeof maxLon === 'undefined' ||
		        maxLon > coord.lon)
			maxLon = coord.lon;
		});
		map.fitBounds([
		    [minLat, minLon],
		    [maxLat, maxLon]
		]);
	    };
	}
    };
});

app.directive('pdfJs', function() {
    return {
	restrict: 'A',
	link: function($scope, element, attrs) {
	    $scope.setPDF = function(url) {
		// element.attr('src',
		//     "http://mozilla.github.io/pdf.js/web/viewer.html?file=" +
		//     encodeURIComponent(url)
		// );
		element.attr('src',
		    "/static/pdf.js/viewer.html?file=" +
		    encodeURIComponent(url)
		);
	    };
	}
    };
});

app.controller('MainController', function($scope, $http) {
    function getDocsByLoc(lon, lat, cb) {
	$http.get("/documents-by-location/" + lon + "/" + lat).
	    success(cb);
    }

    $scope.onClickMap = function(lon, lat) {
	var t1 = Date.now();
	getDocsByLoc(lon, lat, function(docs) {
	    var t2 = Date.now();
	    console.log("Got", docs.length, "docs in", t2 - t1, "ms");
	    if (docs.length < 1)
		return;
	    $scope.setDocs(docs);
	});
    };

    $scope.docs = [];
    $scope.setDocs = function(docs) {
	$scope.setMarkers(docs);
	var docsSeen = {};
	$scope.docs = [];
	docs.forEach(function(doc) {
	    if (!docsSeen.hasOwnProperty(doc.path)) {
		docsSeen[doc.path] = true;
		$scope.docs.push(doc);
	    }
	});
    };

    $scope.onClickPDF = function(path) {
	$scope.setPDF(window.location.origin + "/pdf/" + path);
    };
});
