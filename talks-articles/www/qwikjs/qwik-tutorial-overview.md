
## Qwik Tutorial

> [source](https://qwik.builder.io/tutorial/),
> [playground](https://qwik.builder.io/playground) lets you have deeper insight into constructs

* Qwik, front-end f/w to build resumable app. Focus to fetch & exec only strictly necessary code for user actions.

> Usable sample available in [qwik-sample-app](https://github.com/abhishekkr/qwik-sample-app) at `http://localhost:5173/samples/`.

### Simple App

> Create a component with simple input slate; store data; attach listeners & fetch data.

* Creating component

```
import { component$ } from '@builder.io/qwik';

export default component$(() => {
  return (
    <main>
      <p>
        <label>GitHub organization: <input value="abhishekkr" /></label>
      </p>
      <section>
        <ul>
          <li><a href="https://github.com/abhishekkr/qwik-sample-app">qwik-sample-app</a></li>
        </ul>
      </section>
    </main>
  );
});
```

* State Management: `useStore` stores component state when assignment is wrapped with it. Present state as a proxy that observes r/w to store; serializes store into json on app pause. Creates a store subscription for properties used in template, which auto update the template.

```
import { component$, useStore } from '@builder.io/qwik';

export default component$(() => {
  const github = useStore({
      org: 'abhishekkr',
      repos: ['qwik-sample-app', 'dotfiles'] as string[] | null,
  });

  return (
    <main>
      <p>
        <label>GitHub org/user: <input value="{github.org}" /></label>
      </p>
      <section>
        {github.repos ? (
          <ul>
            {github.repos.map((repo) => (
              <li><a href={`https://github.com/${github.org}/${repo}`}>{github.org}/{repo}</a></li>
            ))}
          </ul>
        ) : ('loading...')
        }
      </section>
    </main>
  );
});
```

* Event Listeners: like adding `onInput$` property to `<input/>` element triggers re-render on value change.

```
...
    <main>
      <p>
        <label>GitHub org/user:
          <input value="{github.org}" onInput$={(ev) => (github.org = (ev.target as HTMLInputElement).value)} />
        </label>
      </p>
      <section>
...
```

* Fetch resource on state change; `useResource` & `<Resource/>` to help fetch & display data from server. Fetching app is in 1of3 states `pending`, `resolved` & `rejected`.

```
import { component$, useStore, Resource, useResource$ } from '@builder.io/qwik';

export default component$(() => {
  const github = useStore({org: 'abhishekkr'});

  const reposResource = useResource$<string[]>(async ({ track, cleanup }) => {
    track(() => github.org);
    const controller = new AbortController();  // abort if re-runs request received
    cleanup(() => controller.abort());
    return getRepositories(github.org, controller);
  });

  return (
    <main>
      <p>
        <label>GitHub org/user:&nbsp;
          <input value={github.org} onInput$={(ev) => (github.org = (ev.target as HTMLInputElement).value)} />
        </label>
      </p>
      <section>
        <ul>
          <Resource
            value={reposResource}
            onPending={() => <p>loading...</p>}
            onRejected={(error) => <>Error: {error.message}</>}
            onResolved={(repos) => (
              <ul>
                {repos.map((repo) => (
                  <li>
                    <a href={`https://github.com/${github.org}/${repo}`}>{repo}</a>
                  </li>
                ))}
              </ul>
            )}
          />
        </ul>
      </section>
    </main>
  );
});

export async function getRepositories(username: string, controller?: AbortController): Promise<string[]> {
  console.log('FETCH', `https://api.github.com/users/${username}/repos`);
  const resp = await fetch(`https://api.github.com/users/${username}/repos`, {
    signal: controller?.signal,
  });
  const json = await resp.json();
  return Array.isArray(json) ? json.map((repo: { name: string }) => repo.name) : Promise.reject(json);
}
```

> * `track` triggers re-fetch on change; `AbortController` aborts current fetch if a new request raised.
> * `<Resource/>` on Server skips `onPending` state.


### Components

* Building blocks declared using `component$()`, minimally returning JSX element.

* `{expr}` injects data into JSX template. Components are composeable.

```
import { component$ } from '@builder.io/qwik';

export default component$(() => {
  return (<main> <Greeter/> </main>);
});

export const Greeter = component$(() => {
  return <div>Hello World!</div>;
});
```

* Qwik is Resumable, so don't need much pre-requisite to load beforehand. Each components generates a Symbol that is downloaded on demand.

* To link loading of tightly coupled components, `Inline Component` can be used. Inline component can't have `use*` methods and can't project content with `<Slot/>`.

> In above example `Greeter` becomes inline by simply removing `component$()` usage.

```
...
export const Greeter = (() => {
...
```


### Events

* Qwik listen varied events with `on[EventName]$` on element subscribing to browser events.

* Every `$` use indicates lasy-load optimization into a separate chunk. TS rules ensure `$` occurences aren't skipped. Qwikloader scripts get inlined into HTML & a global listener is set-up for all browser events. Qwikloader intercepts `on:click` attribute which has js chunk URL to fetch (gets pre-fetched), gets executed. This brings resumability.

```
import { component$ } from '@builder.io/qwik';

export default component$(() => {
  return <button onClick$={() => alert('Hello World!')}>Click Me</button>;
});
```

* Qwik has `document:` & `window:` ns prefixes to listen global events.

```
export default component$(() => {
  const store = useStore({ x: 0, y: 0 });
  const docStore = useStore({ x: 0, y: 0 });
  return (
    <>
        <div onMouseMove$={(event) => { store.x = event.clientX; store.y = event.clientY; console.log(store); }} >
        Your mouse location is ({store.x}, {store.y}).
        </div>
        <div onMouseMove$={(event) => { docStore.x = event.clientX; docStore.y = event.clientY; console.log(docStore); }} >
        Your mouse location is ({docStore.x}, {docStore.y}).
        </div>
    </>
  );
});
```

* Since general Qwik execution model is async & event handler to prevent default is needed beforehand. Qwik Event API has `preventDefault()` to avoid default behaviors like a:href clicks.

```
<a href="/" preventdefault:click onClick$={() => whatever()}> click me! </a>
```

* Sync calls need explicit eager loading of event handler.

> * `useTask$()` (server, on-track browser) should be first choice for async/sync component init & state changes.
> * If not viable, then rely on `useVisibleTask$()` (for forced client-side eval) or `useResource$()` (for async data fetch & signal to not block render).
> * If just need to synchronously compute new state fro existing, via `useComputed$()` method.

```
import { component$, useVisibleTask$, useSignal } from '@builder.io/qwik';

export default component$(() => {
  const popupHome = useSignal<Element>();
  useVisibleTask$(() => {
    // Typically these are used for complex client side JS calls.
    const handler = (e: Event) => { e.preventDefault(); window.open('/'); };
    popupHome.value!.addEventListener('click', handler);
    return () => popupHome.value!.removeEventListener('click', handler);
  });

  return (
    <>
      <a href="/" ref={popupHome}>click me!</a>
      <a href="/" onClick$={() => window.open('/')}>click me!</a>
    </>
  );
});
```

* Listeners: `useOn()` for current component's root element; `useOnDocumente()` for 'document' object; `useOnWindow()`.

> `useOn()`'s 2nd QRL (Qwik URL as lazy-load ref). If 2nd arg is a function instead of QRL, would eagerly execute to allocate listener closure. Using `$` as below, Qwik can lazy load the closure as listener triggers.

```
import { component$, useOn, $ } from '@builder.io/qwik';

export default component$(() => {
  useOn('click', $(() => alert('Hello World!')));
  return <p>App Component. Click me.</p>;
});
```


### Stores

* Qwik tracks app state to serialize (on pauses) & deserialize (on client resume); also to create subscriptions on components to re-render. E.g. as `useStore` used above to track mouse's (X,Y) coords.

> * `<script type='qwik/json' ..>` block would help debug serialization in browser. The `json` block keeps `objs` list as key with first element as data & a `subs` list as key for subscriptions.
> * State is not coupled with component creating it; can be passed to component and create required subs.

* `useStore` tracks deep reactivity; Arrays/Objects in it are also reactive. To track nested props, `useStore` need to allocate several `Proxy` object adding load. Passing `{deep: false}` to `useStore` helps track at top-level only.

> * Reactivity helps `track` subscribed components to state; so as to invalidate only relevant component for selective re-render.
> * Initial server render puts Proxy (created by `useStore` for a state) in learn mode.

```
...
  const dat = useStore({ counter: { count: 0 }, list: [0] });
  const shallowDat = useStore({ counter: { count: 0 }, list: [0] }, { deep: false });

  return (
    <>
      <div>
        <Display counter={dat.counter} list={dat.list} />
        <button onClick$={() => dat.counter.count++}>+1 Count</button>
        <button onClick$={() => dat.list.push(0)}>+1 List element</button>
      </div>
      <div>
        <Display counter={shallowDat.counter} list={shallowDat.list} />
        <button onClick$={() => (shallowDat.counter = { count: ++shallowDat.counter.count })}>+1 Count</button>
        <button onClick$={() => (shallowDat.list = [...shallowDat.list, 0])}>+1 List element</button>
      </div>
...
```

> Above example shows how `shallowDat` has to reassing top-level element for subscription to trigger.

* Stores can also include QRLs. Can have cycle inferences. E.g. as below

```
import { component$, useStore, type QRL, $ } from '@builder.io/qwik';

interface ParentStore {
  name: string;
  children: ChildStore[];
  greet: QRL<(p: ParentStore) => void>;
}
interface ChildStore { name: string; p: ParentStore; }
export default component$(() => {
  const alice: ParentStore = {name: 'Alice', children: [], greet: $((p) => alert(p.name))};
  alice.children = [{ name: 'Bob', p: alice }, { name: 'Eve', p: alice }];
  const frnds = useStore<ParentStore>(alice, { deep: true });
  return (
    <>
      {frnds.name} <button onClick$={async () => await frnds.greet(alice)}>Greet</button>
      <ul>
        {frnds.children.map((child) => (
          <li>{child.name} is a friend of {child.p.name}&nbsp;
            <button onClick$={async () => await frnds.greet(child)}>Greet</button>
          </li>
        ))}
      </ul>
    </>
  );
});
```

* Non-Serializable values can be stored only while app is resumed; discarded as soon paused.

```
import { component$, type NoSerialize, useStore } from '@builder.io/qwik';

interface AppStore {
  time: null | string;
  cleanup: NoSerialize<() => void>;
}
export default component$(() => {
  const store = useStore<AppStore>({ time: null, cleanup: undefined });
  return (
    <>
      <div>Current Time: {dat.time}</div>
      <button
        onClick$={() => {
          dat.time = new Date().toString();
          const id = setInterval(() => (dat.time = new Date().toString()), 1000);
          dat.cleanup = noSerialize(() => clearInterval(id));
        }}
      > start </button>
      <button
        onClick$={() => {
          dat.time = null;
          dat.cleanup && dat.cleanup();
          dat.cleanup = undefined;
        }}
      > stop </button>
    </>
  );
});
```

> Above `dat.cleanup()` wraps `clearInterval()` within `noSerialize()`.


### Component Props

* Components have props as args to fn; used to pass data to children component.

```
... <Greeter salutation="Hello" name="World" />
...
interface GreeterProps { salutation: string; name: string; }
export const Greeter = component$((props: GreeterProps) => {
  return (
    <p> {props.salutation} {props.name}! </p>
  );
});
```

* Props must be serializable; Callback Fn ain't directly so to pass them convert to QRLs using `$()`.

> * Qwik's convenience APIs end in `$`. Same as explicit `cbQrl = $(() => {..}); useTaskQrl(cbQrl)`; helps separate declartion and invoke blocks. Otherwise can use inline form `useTask$(() => {...}/>`.
> * The prop name to end in `$`.

```
  const helloQrl = $(() => { return 'Hello'; });
...
  <Greeter salutation$={helloQrl} name$={() => {alert("World");}} />
...
interface GreeterProps {
  salutation$: PropFunction<() => void>;
  name$: PropFunction<() => void>;
}
export const Greeter = component$((props: GreeterProps) => {
  return (
    <div> <button onClick$={props.name$}>{props.salutation$()}</button> </div>
  );
});
```


### Reactivity

* Implicit: template. Subscriptions are auto created & removed. In SSR, subscription info generates at server and gets passed to client. E.g. use of Stores before.

* Explicit: useTask$. Allows explicit trigger of code on a property change. `useTask$` hooks execute before renders; can be async.

* Explicit: useResource$; to integrate with external resources for data fetch and prepare. Provides `<Resource/>` to display data element. E.g. usage as for github repo example.


### Context

* Can pass data to child components without explicit params via Context sharing. Used for data like styling, app state, user management, etc.

> Using context has 3 stages:
> * `createContextId()` to create a unique serializable ID.
> * `useContextProvider()` in parent component to publish context value.
> * `useContext()` in all (grand)children components to retrieve context.
> Code available as `component SampleCtx` in repo link at top.

### Lifecycle Hooks

* `useTask$`, e.g. code available as `component SampleTask` in repo link at top.

> * Can also have clean-up fn to run on next invoke or component removal.
> * Creates subscription using `track()` fn; each invoke clears subscription so based on requirement would need to be reset.
> * If not async, get eval before render and thus can compute values used in render itself.

* `useSignal$` stores single value as `useStore()`; can skip re-renders.

> * Works with all primitive types; `useStore` is suggested for lists or complex objects.
> * E.g. `const intStore = useSignal(0);` to create and accessible by 'value' prop as `intStore.value++`.
> * Can hold ref of DOM elements created by component, via prop binding `ref={signalIdent}` in element. Code e.g. in `component SampleSignal` at repo link atop.

* `useVisibleTask$` evals on client after it resumes; recieves `track` like `useTask$`.

> Takes optional arg `{strategy: val}`.
> * Where default val `'intersection=observer'` first exec when element is visible in viewport;
> * `'document-ready'` using document load event; and
> * `'document-idle'` using requestIdleCallback API.
> Code e.g. in `component Clock` at repo link atop.

* `useOn$`, `useOnDocumente$`, `useOnWindow$` to attach listeners. Useful with custom APIs or for dynamic events based on props. Code e.g. in `Component MouseXY` at repo sample.

* `useUnMount$` is planned to run cleanup code on component removal.


### Projection

* Projection is parent component deciding content & passing it to child to decide if & how to render.

> * `<Panel>` element within `<App>` is content to be projected. Wraps it in `<div>` tag and project using `<Slot>` element.
> * `<Slot>` allows out-of-order render, as even if parent is not yet resumed. Whereas Child component needs to be serializable & need to be re-rendered every time parent does.
> Code e.g. in `component SampleSlot` at repo link atop.

* More than one content `<Slot name="alpha">` may need be projected for `<div q:slot="alpha">...</div>`; done via Named Slots.

> In e.g. `component SampleNamedSlot`, used `<Collapsible>` toggles b/w open/closed states.. doesn't need re-render to recover projected content.

* Fallback Content allows children to display something if parent didn't. Managed via CSS selectors for tag wrapping slot. Code e.g. is `<Slot name="notset" />` in sample link atop.


### Styling

* `useStyles$` loads style while mounting component. If a child component is loaded later, new styles get inserted alongwith. Code Sample is part of `component SampleNamedSlot`.

> * CSS from a separate file can be imported to be used as `import AppCSS from './app.css'`.

* `useStylesScoped$` allows to load & apply style to specific component scope only. As applied for `div` in sample code `component Panel` in repo link atop, only applies to that component.


### $ and QRL

* Optimizer looks for `$` suffixed Fn, transform it to entry point. Becomes a lazy-loaded boundary in itself.

> * Optimizer can serialize all that Qwik can. Special handling for Closures making it serializable & thus Qwik very resumable.

* Using `$()` fn to mark data makes it a lazy-loading constant, returning a QRL which is serializable. Code sample in `component Collapsible` at repo link atop.

* Closures can be lazy-loading similarly within `$(..)` generating `QRL<Fn>`.

```
  const store = useStore({ name: '' });
...
  <input
    // // Instead of commented segment, can be written easily as below
    // onInput$={$(async (event: KeyboardEvent) => { ...
    // })}
    onInput$={(event) => {
      const input = event.target as HTMLInputElement; store.name = input.value;
    }}
    value={store.name}
  />
```


### Composing new APIs

* Creating API with $ suffix. E.g. a lazy-loaded alternative for `setTimeout(() => {..}, timeout);`.

```
export function delayQrl<T>(fn: QRL<() => T>, delayInMs: number): Promise<T> {
  return new Promise((res) => {
    setTimeout(() => {
      res(fn.invoke());
    }, delayInMs);
  });
}

export const delay$ = implicit$FirstArg(delayQrl);
...
    <button onClick$={async () => { s.count++; await delay$(() => s.delay++, 1000); }} >+1</button>
```

* Hooks allow abstract component's common logic, to be shared. Code e.g. as `useMousePosition()` in sample at repo link atop.


### Understanding Qwik Difference

* Qwik only loads component in client that is to be re-rendered; static don't need to. Data binding determines if a component is static or dynamic. Static components never need to be loaded from client, only reachable on initial render at SSR.

* Qwik tree shakes stores needed by client. To have less states managed & sent to client; using them such that not entire store is passed.. helps Qwik make intelligent decision.

---
