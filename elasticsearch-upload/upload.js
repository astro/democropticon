var fs = require('fs');
var async = require('async');
var cheerio = require('cheerio');
var elasticsearch = require('elasticsearch');
var es = new elasticsearch.Client({
    host: 'localhost:9200',
    log: 'warning'
});

var dataDir = process.argv[2];
if (!dataDir) {
    throw new Error("Pass dataDir!");
}

var INDEX_NAME = 'ratsinfo'

function uploadSession(sessionDir, cb) {
    var metadata
    function getDocDescriptions(filename) {
        var result
        metadata.documents.forEach(function(document) {
            if (document.file_name === filename) {
                // No part, no template_id
                result = {
                    description: document.description,
                    session_description: metadata.description,
                    session_id: metadata.id,
                    started_at: metadata.started_at
                }
            }
        })

        if (result) {
            return result
        } else {
            metadata.parts.forEach(function(part) {
                part.documents.forEach(function(document) {
                    if (document.file_name === filename) {
                        result = {
                            description: document.description,
                            template_description: part.description,
                            template_id: part.template_id,
                            session_description: metadata.description,
                            session_id: metadata.id,
                            started_at: metadata.started_at
                        }
                    }
                })
            })

            if (result) {
                return result
            } else {
                throw "Cannot get description for " + sessionDir + "/" + filename
            }
        }
    }

    async.waterfall([function(cb) {
        fs.readFile(sessionDir + "/metadata.json", function(err, data) {
            if (data) {
                metadata = JSON.parse(data);
            } else {
                console.error("No metadata in ", sessionDir, ":", err && err.stack || err);
            }
            cb();
        });
    }, function(cb) {
        if (!metadata) return cb()

        es.index({
            index: INDEX_NAME,
            type: 'session',
            id: sessionDir,
            body: metadata
        }, function(err) {
            cb(err)
        });
    }, function(cb) {
        fs.readdir(sessionDir, cb);
    }, function(files, cb) {
        if (!metadata) return cb()

        async.forEachSeries(files, function(file, cb) {
            if (!/\.xml$/.test(file)) return cb()

            fs.readFile(sessionDir + "/" + file, {
                encoding: 'utf8'
            }, function(err, data) {
                if (err) return cb(err)

                // Get HTML text
                try {
                    var $ = cheerio.load(data)
                    var text = $('body').text()
                } catch (e) {
                    console.error(e.message);
                    return cb();
                }

                // Annotate with session metadata

                // Restore original extension:
                file = file.replace(/\.xml$/, ".pdf")
                var doc = {}
                try {
                    // Obtain doc-specific metadata
                    doc = getDocDescriptions(file)
                } catch (e) {
                    console.error(e.message);
                    return cb();
                }
                doc.file_name = file
                doc.text = text
                es.index({
                    index: INDEX_NAME,
                    type: 'pdf',
                    id: file.replace(/\.xml$/, ""),
                    body: doc,
                    parent: metadata.id
                }, cb);
            })
        }, cb)
    }], cb)
}

/*** Main function ***/
async.waterfall([function(cb) {
    fs.readdir(dataDir, cb);
}, function(subDirs, cb) {
    subDirs = subDirs.filter(function(subDir) {
        return /^\d+$/.test(subDir);
    });

    var i = 0;
    // For each session in the dataDir uploadSession() in parallel:
    async.eachLimit(subDirs, 4, function(subDir, done) {
        i++;
        console.log(Math.ceil(100 * i / subDirs.length) + "% " + subDir);
        var sessionDir = dataDir + "/" + subDir;
        uploadSession(sessionDir, done)
    }, cb);
}, function(cb) {
    es.indices.optimize({ index: INDEX_NAME }, cb);
}], function(err) {
    if (err) {
        console.error(err.stack || err);
    }
});
