import Fastify from 'fastify'
const fastify = Fastify({
  logger: false
})

let port = 3001;
if (process.argv.length >= 3) {
  port = parseInt(process.argv[2], 10) ?? 3001;
}

// Declare a route
fastify.get('/', async function handler(request, reply) {
  return "hello, world"
});

// Run the server!
try {
  console.log("fastify port: " + port)
  await fastify.listen({ port: port })
} catch (err) {
  fastify.log.error(err)
  process.exit(1)
}
