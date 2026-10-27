// TienLen service worker: cache-first for the app shell so the PWA
// launches instantly (and offline), revalidating in the background so
// deploys are picked up on the next launch.
const CACHE = 'tienlen-v1';
const SHELL = [
    './',
    './client.js',
    './icon.svg',
    './icon-192.png',
    './icon-512.png',
    './apple-touch-icon.png',
];

self.addEventListener('install', (event) => {
    event.waitUntil(
        caches.open(CACHE).then((cache) => cache.addAll(SHELL))
    );
    self.skipWaiting();
});

self.addEventListener('activate', (event) => {
    event.waitUntil(
        caches.keys().then((keys) =>
            Promise.all(keys.filter((k) => k !== CACHE).map((k) => caches.delete(k)))
        ).then(() => self.clients.claim())
    );
});

self.addEventListener('fetch', (event) => {
    const url = new URL(event.request.url);
    // Never cache: the websocket, API endpoints, the manifest (it is
    // dynamic - it bakes in the room code), and anything with a query
    // string (room code links, admin keys).
    if (url.pathname.endsWith('/ws')
        || url.pathname.includes('/admin')
        || url.pathname.endsWith('/manifest')
        || url.pathname.endsWith('/check')
        || url.search) {
        return;
    }
    // Same-origin only.
    if (url.origin !== self.location.origin) return;

    // Stale-while-revalidate: serve from cache immediately, refresh
    // the cache in the background so the next launch has the new code.
    event.respondWith(
        caches.match(event.request).then((cached) => {
            const refresh = fetch(event.request).then((response) => {
                if (response.ok) {
                    const copy = response.clone();
                    caches.open(CACHE).then((cache) => cache.put(event.request, copy));
                }
                return response;
            }).catch(() => cached);
            return cached || refresh;
        })
    );
});
