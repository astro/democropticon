var fs = require('fs');
var async = require('async');
var elasticsearch = require('elasticsearch');
var es = new elasticsearch.Client({
    host: 'localhost:9200',
    log: 'warning'
});

var dataDir = process.argv[2];
if (!dataDir) {
    throw new Error("Pass dataDir!");
}

function extract_documents(metadata) {
    var docs = metadata.documents ?
        metadata.documents.filter(function(doc) {
            return !!doc.file_name;
        }) :
        [];
    /* Recurse into `parts' key */
    if (metadata.parts) {
        metadata.parts.forEach(function(part) {
            docs = docs.concat(extract_documents(part));
        });
    }
    docs.forEach(function(doc) {
        if (!doc.context) {
            doc.context = [];
        }
        var c = { description: metadata.description };
        if (metadata.decision) {
            c.decision = metadata.decision;
        }
        doc.context.push(c);
    });
    return docs;
}

async.waterfall([function(cb) {
    es.indices.putMapping({
        index: 'ratsinfo',
        type: 'pdf',
        body: {
            pdf: {
                _source: { excludes: ["text"] },
                properties: {
                    file_name: {
                        type: 'string',
                        index: 'no'
                    },
                    text: {
                        type: 'string',
                        store: false,
                        index_name: 'text'
                    },
                    locations: {
                        type: 'geo_point',
                        lat_lon: true,
                        store: true,
                        index_name: 'location'
                    },
                    description: {
                        type: 'string',
                        store: true,
                        index_name: 'description'
                    },
                    context: {
                        properties: {
                            description: {
                                type: 'string',
                                store: true,
                                index_name: 'description'
                            },
                            decision: {
                                type: 'string',
                                index: 'no',
                                store: true
                            }
                        }
                    }
                }
            }
        }
    }, function(err) {
        cb(err);
    });
}, function(cb) {
    fs.readdir(dataDir, cb);
}, function(subDirs, cb) {
    subDirs = subDirs.filter(function(subDir) {
        return /^\d+$/.test(subDir);
    });

    var i = 0;
    async.eachLimit(subDirs, 4, function(subDir, done) {
        i++;
        console.log(Math.ceil(100 * i / subDirs.length) + "% " + subDir);
        var path = dataDir + "/" + subDir;
        var pdfFiles, metadata, locations = {}, docs = [];
        async.waterfall([function(cb) {
            fs.readdir(path, cb);
        }, function(files, cb) {
            pdfFiles = files.filter(function(f) {
                return /\.pdf$/i.test(f);
            });

            fs.readFile(path + "/metadata.json", function(err, data) {
                if (data) {
                    metadata = JSON.parse(data);
                } else {
                    console.error("No metadata in ", path, ":", err && err.stack || err);
                }
                cb();
            });
        }, function(cb) {
            fs.readFile(path + "/locations.json", function(err, data) {
                if (data) {
                    JSON.parse(data).forEach(function(item) {
                        locations[item.file_name] = item.coordinates;
                    });
                } else {
                    console.error("No locations in ", path, ":", err && err.stack || err);
                }
                cb();
            });
        }, function(cb) {
            /* Gather docs from metadata */
            if (metadata) {
                docs = extract_documents(metadata);
            }

            /* Anything left? */
            var unknownFiles = pdfFiles.filter(function(file) {
                return !docs.some(function(doc) {
                    return doc.file_name === file;
                });
            });
            if (unknownFiles.length > 0) {
                console.log("Adding", unknownFiles.length, "unknown documents in", path, ":", unknownFiles.join(" "));
                unknownFiles.forEach(function(file) {
                    docs.push({ file_name: file });
                });
            }

            /* Attach session metadata */
            var sessionMetadata = metadata && {
                description: metadata.description,
                started_at: metadata.started_at,
                ended_at: metadata.ended_at,
                committee: metadata.committee
            };
            docs.forEach(function(doc) {
                doc.id = subDir + "/" + doc.file_name.replace(/\.pdf$/, "");
                if (metadata) {
                    doc.session = sessionMetadata;
                }
                if (locations[doc.file_name]) {
                    doc.locations = locations[doc.file_name];
                }
            });

            async.forEachSeries(docs, function(doc, cb) {
                var filepath = path + "/" + doc.file_name.replace(/\.pdf$/, ".txt");
                fs.readFile(filepath, {
                    encoding: 'utf8'
                }, function(err, text) {
                    if (typeof text != 'string') {
                        console.error("Cannot read text file", filepath, ":", err && err.stack || err);
                    }

                    var body = {
                        file_name: doc.file_name
                    };
                    function docToBody(key) {
                        if (doc[key]) {
                            body[key] = doc[key];
                        }
                    }
                    docToBody('description');
                    docToBody('session');
                    docToBody('context');
                    docToBody('locations');
                    docToBody('pdf_metadata');
                    if (text) {
                        body.text = text;
                    };
                    es.index({
                        index: 'ratsinfo',
                        type: 'pdf',
                        id: doc.id,
                        body: body
                    }, cb);
                });
            }, cb);
        }], done);
    }, cb);
}, function(cb) {
    es.indices.optimize({ index: 'ratsinfo' }, cb);
}], function(err) {
    if (err) {
        console.error(err.stack || err);
    }
});
