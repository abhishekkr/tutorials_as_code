
## Escaping the Sandbox by not breaking it

> by Qidan He, Marco Grassi
> at DefCon 24 Las Vegas


### Couple of Sandbox Implementations

#### DAC (Discretionary Access Control)

Android base Sandbox mechanism from initial version, standard linux process isolation with unique UIDs and GIDs specifying capabilities.


#### MAC (Mandatory Access Control)

Specify access to resource based with decision policy. Example SeLinux policy subjected process.
Introduced in Android at v4.3 officially, became enforcing short after.

> If MAC rejected, DAC is not even considered.


#### Safari Sandbox

```
  ,-----------------------------------------,
  |                 UI Process              |
  | ,-------------------------------------, |
  | |            Application              | |
  | ,-------------------------------------, |
<---------------------------------------------API Library
  | ,-------------------------------------, |
  | |            WebKit (UI Process)      | |
  | ,-------------------------------------, |
  |-----------------------------------------|
  | |            WebKit (Web Process)     | |
  | ,-------------------------------------, |
  | |            WebCore                  | |
  | ,-------------------------------------, |
  | |            JS Engine                | |
  | ,-------------------------------------, |
  |              Web Process                |
  '-----------------------------------------'

```


#### 



---
