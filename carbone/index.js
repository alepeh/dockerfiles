const express = require('express')
const app = express()
const bodyParser = require('body-parser');
const port = 3000
const carbone = require('carbone')
const fs = require('fs');
const randomstring = require("randomstring");

app.use(bodyParser.json({ extended: true }));

app.post('/render', (req, res) => {
    var data = req.body.payload;
    var templateFile = req.body.metadata.template.key;
    var filename = randomstring.generate(10) + '.pdf';
      var options = {
        convertTo : 'pdf' //can be docx, txt, ...
      };
    
      carbone.render('./templates/' + templateFile, data, options, function(err, result){
        if (err) return console.log(err);
        fs.writeFileSync('/tmp/' + filename, result);
        const contentsBase64 = fs.readFileSync('/tmp/' + filename, {encoding: 'base64'});
        res.send(contentsBase64);
      });
})

app.listen(port, '0.0.0.0', () => {
  console.log(`Carbone app listening at http://localhost:${port}`)
})