import { createServer } from "node:http";
import { readFile, stat } from "node:fs/promises";
import { extname, resolve, sep } from "node:path";

const [rootArg, htmlArg, contentArg, portArg] = process.argv.slice(2);
if (!rootArg || !htmlArg || !contentArg || !portArg) {
  throw new Error("usage: markdown-preview-server.mjs ROOT HTML CONTENT PORT");
}

const root = resolve(rootArg);
const html = resolve(htmlArg);
const content = resolve(contentArg);
const port = Number(portArg);
const types = {
  ".css": "text/css; charset=utf-8",
  ".gif": "image/gif",
  ".jpeg": "image/jpeg",
  ".jpg": "image/jpeg",
  ".png": "image/png",
  ".svg": "image/svg+xml",
  ".webp": "image/webp"
};

function insideRoot(path) {
  return path === root || path.startsWith(`${root}${sep}`);
}

createServer(async (request, response) => {
  try {
    const url = new URL(request.url, `http://${request.headers.host}`);
    if (url.pathname === "/" || url.pathname === "/index.html") {
      response.setHeader("Content-Type", "text/html; charset=utf-8");
      response.end(await readFile(html));
      return;
    }
    if (url.pathname === "/content.html") {
      response.setHeader("Content-Type", "text/html; charset=utf-8");
      response.setHeader("Cache-Control", "no-store");
      response.end(await readFile(content));
      return;
    }

    const file = resolve(root, `.${decodeURIComponent(url.pathname)}`);
    if (!insideRoot(file) || !(await stat(file)).isFile()) {
      response.writeHead(404).end("Not found");
      return;
    }

    response.setHeader("Content-Type", types[extname(file).toLowerCase()] ?? "application/octet-stream");
    response.end(await readFile(file));
  } catch (error) {
    response.writeHead(error.code === "ENOENT" ? 404 : 500).end(error.message);
  }
}).listen(port, "127.0.0.1", () => {
  console.log(`READY http://127.0.0.1:${port}`);
});
