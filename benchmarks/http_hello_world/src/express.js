import express from "express";

const app = express();
const port = 3002;

app.get("/", (req, res) => {
  res.send("Hello World!");
});

app.listen(port, () => {
  console.log(`Express server listening on port ${port}`);
});
