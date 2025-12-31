import express from "express";

const app = express();
let port = 3002;
if (process.argv.length >= 3) {
  port = parseInt(process.argv[2], 10) ?? 3002;
}

app.get("/", (req, res) => {
  res.send("Hello World!");
});

app.listen(port, () => {
  console.log(`Express server listening on port ${port}`);
});
