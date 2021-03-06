var docSource = require('./lib/doc_source')
var cheerio = require('cheerio')
var stem = require('snowball-german')

var dataDir = process.argv[2];
if (!dataDir) {
    throw new Error("Pass dataDir!");
}

var tokenHits = {}
var tokenSpellings = {}
var maxMatches = 0
var docCount = 0

docSource(dataDir, function(session) {
    console.log("session", session.description)
}, handleDoc, function(err) {
    if (err) {
        console.error(err.stack || err.message)
        process.exit(1)
    }

    runSummary()
})

function handleDoc(doc) {
    var $ = cheerio.load(doc)
    var body = $('body').text()
    var tokens = {}
    body.split(/[\s,\.;:\?!\/ ]+/g).forEach(function(token) {
        token = token.toLowerCase()
        var stemmed = stem(token)
        tokens[stemmed] = true

        if (!tokenHits.hasOwnProperty(stemmed)) {
            tokenHits[stemmed] = { matches: 0, docs: 0 }
        }
        tokenHits[stemmed].matches += 1
        if (tokenHits[stemmed].matches > maxMatches) {
            maxMatches = tokenHits[stemmed].matches
        }

        if (!tokenSpellings.hasOwnProperty(stemmed)) {
            tokenSpellings[stemmed] = {}
        }
        if (!tokenSpellings[stemmed].hasOwnProperty(token)) {
            tokenSpellings[stemmed][token] = 0
        }
        tokenSpellings[stemmed][token] += 1
    })
    tokens = Object.keys(tokens)
    // console.log("tokens", tokens)
    tokens.forEach(function(token) {
        tokenHits[token].docs += 1
    })
    docCount += 1
}

function tfidf(hit) {
    var tf = 0.5 + 0.5 * hit.matches / maxMatches
    var idf = Math.log(docCount / hit.docs)
    return tf * idf
}

function runSummary() {
    console.log("Preparing tokens")
    var tokens = Object.keys(tokenHits)
    tokens.forEach(function(token) {
        tokenHits[token].tfidf = tfidf(tokenHits[token])
    })
    console.log("Sorting...", tokens.length, "tokens")
    topTokens = tokens.sort(function(token1, token2) {
        return tokenHits[token2].tfidf - tokenHits[token1].tfidf
    }).slice(0, 99)

    topTokens.forEach(function(token) {
        var topSpellings = Object.keys(tokenSpellings[token]).sort(function(spelling1, spelling2) {
            return tokenSpellings[spelling2] - tokenSpellings[spelling1]
        }).slice(0, 9)
        var hit = tokenHits[token]
        console.log([
            token,
            hit.tfidf,
            hit.matches,
            hit.docs,
            topSpellings.join(" ")
        ].join("\t"))
    })
}
