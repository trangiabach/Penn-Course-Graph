const winkNLP = require( 'wink-nlp');
const model = require( 'wink-eng-lite-web-model' );
const similarity = require('wink-nlp/utilities/similarity.js');
const nlp = winkNLP( model );
const its = nlp.its;
const as = nlp.as;

const data = require('./data.json')

Object.keys(data).forEach(key => {
    data[key].embedding = nlp.readDoc(data[key].description).tokens().out(its.value, as.bow)
})

var fs = require('fs');
fs.writeFile ("newGraph.json", JSON.stringify(data), function(err) {
    if (err) throw err;
    console.log('complete');
    }
);

