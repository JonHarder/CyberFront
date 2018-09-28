const express = require('express');
const path = require('path');

const app = express();

app.get('/', (req, res) => {
    res.sendFile(path.join(__dirname, './src/index.html'));
});

app.get('/elm.js', (req, res) => {
    res.sendFile(path.join(__dirname, './src/elm.js'));
});

app.get('/index.js', (req, res) => {
    res.sendFile(path.join(__dirname, './src/index.js'));
});

app.get('/assets/spritemap.svg', (req, res) => {
    res.sendFile(path.join(__dirname, './src/assets/spritemap.svg'));
});

app.listen(8000);
