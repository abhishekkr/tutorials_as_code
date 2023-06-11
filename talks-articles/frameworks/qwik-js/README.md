
## Qwik

> JS, app framework by Builder.io, [source](https://qwik.builder.io/)

* Qwik has familiar dev experience to author components; while delivering Live HTML without Hydration it uses Resumability, Designed from ground-up to address size problem with Delay Loading of JS.

> * Qwik app only needs 1KB JS to become interactive. Aggresively delays app download and execution, providing near instant startup perf.
>
> * Resumability allows Qwik app to continue execution where server left off. It serializes listeners, internal data structures & app state into HTML during server-browser handoff. With all details serialized, the client can just resume execution where server left off.
>
> * Due to [Resumability](https://qwik.builder.io/docs/concepts/resumable/), Qwik app at any point in lifecycle can be serialized and moved to a different VM (in cases of webapp from server to browser). Execution just resumes without need of Hydration.
>
> * Qwik uses JSX as template syntax for familiar experience; but is not React under the hood. Has Serverless & SSG deployment support. Easy MPA; also has `<Link>` component that triggers SPA navigation.


* Getting Started could be tracked as [sample project](https://github.com/abhishekkr/qwik-sample-app); with all Commit History and [Command Log](https://github.com/abhishekkr/qwik-sample-app/blob/main/COMMAND-LOG.md).


### Project Structure

* Files & dirs in `src/routes/` are by convention mapped to App's URL. Homepage is `src/routes/index.tsx`; `src/routes/layout.tsx` is root layout for entire app. [Qwik City](https://qwik.builder.io/docs/qwikcity/) is Dir based Router in use.

* `src/components/` for reusable pieces is available to all Qwik starters

* `public/` for assets; when built these are copied over to `dist/` and served at root

* `src/entry.ssr.tsx` is SSR Entrypoint; `src/root.tsx` is App Trr Entrypoint (the first component to be rendered); `src/global.css` is global css imported at root.tsx

* `tsconfig.json` for standard TypeScript compiler configs; `vite.config.ts` for Vite configs


---
