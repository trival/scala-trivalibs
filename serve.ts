import { serve } from "bun"
import { readdirSync, statSync } from "node:fs"
import { join } from "node:path"

const examplesDir = "./examples"

const routes: Record<string, Response | (() => Promise<Response>)> = {}

const indexPath = join(examplesDir, "index.html")
routes["/"] = await Bun.file(indexPath).exists()
	? new Response(Bun.file(indexPath), { headers: { "Content-Type": "text/html" } })
	: new Response("missing examples/index.html", { status: 404 })

for (const entry of readdirSync(examplesDir)) {
	const full = join(examplesDir, entry)
	if (!statSync(full).isDirectory()) continue
	if (entry === "out") continue
	const html = join(full, "index.html")
	if (!(await Bun.file(html).exists())) continue
	routes[`/${entry}/`] = new Response(Bun.file(html), {
		headers: { "Content-Type": "text/html" },
	})
}

const server = serve({
	port: Number(process.env.PORT) || 5000,
	routes,
	async fetch(req) {
		const url = new URL(req.url)
		const file = Bun.file(join(examplesDir, url.pathname))
		if (await file.exists()) return new Response(file)
		return new Response("not found", { status: 404 })
	},
	development: true,
})

console.log(`Dev server running at http://localhost:${server.port}`)
