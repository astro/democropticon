var fs = require('fs');
var async = require('async');

module.exports = function(dataDir, sessionCb, docCb, finalCb) {
    async.waterfall([function(cb) {
        console.log("readdir", dataDir + "/sitzungsdokumente-tika")
        fs.readdir(dataDir + "/sitzungsdokumente-tika", cb);
    }, function(subDirs, cb) {
        subDirs = subDirs.filter(function(subDir) {
            return /^\d+$/.test(subDir)
        })  //.slice(0, 10)  // TMP
        cb(null, subDirs)
    }, function(subDirs, cb) {
        var i = 0
        var prevProgress
        async.eachSeries(subDirs, function(subDir, cb) {
            i++;
            var progress = Math.ceil(100 * i / subDirs.length) + "%"
            if (prevProgress !== progress) {
                console.log("[doc_source] " + progress)
                prevProgress = progress
            }

            var sessionDir = dataDir + "/sitzungsdokumente-tika/" + subDir
            async.waterfall([function(cb) {
                fs.readFile(sessionDir + "/metadata.json", cb)
            }, function(data, cb) {
                cb(null, JSON.parse(data))
            }, function(sessionJson, cb) {
                sessionCb(sessionJson)
                cb()
            }, function(cb) {
                fs.readdir(sessionDir, cb);
            }, function(files, cb) {
                async.forEachSeries(files, function(file, cb) {
                    if (!/\.xml$/.test(file)) return cb()

                    fs.readFile(sessionDir + "/" + file, {
                        encoding: 'utf8'
                    }, function(err, data) {
                        if (data) docCb(data)
                        cb(err)
                    })
                }, cb)
            }], function(err) {
                if (err) {
                    console.error("Processing subDir", subDir, err.stack || err.message)
                }
                cb()
            })
        }, cb)
    }], function(err) {
        finalCb(err)
    })
}
