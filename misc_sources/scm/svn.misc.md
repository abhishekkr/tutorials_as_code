## SVN quick reference

#### Basics

* list contents of a path remotely
```
svn list https://svn-repo/svn/
```

* create a new dir remotely
```
svn mkdir https://svn-repo/svn/nudir -m "creating a new dir for example"
```

* checkout a copy of any dir in repo
```
svn co https://svn-repo/svn/nudir
```

* push in changes made to checked out code
```
cd nudir ; mkdir somedir ; touch somedir/somedirfile1
svn add somedir
svn commit -m "adding somedir to example nudir"
```

* update local copy of repo-dir with latest changes pushed
```
svn up
```

* show changes made on repository
```
svn log
```

---
---
