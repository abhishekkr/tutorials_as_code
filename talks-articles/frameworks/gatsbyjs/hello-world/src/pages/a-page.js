import React from "react"
import { Link } from "gatsby"

import Header from "../components/header"
import SubHeader from "../components/subheader"
import Mylinks from "../components/mylinks"

import styles from "./a-page.module.css"

console.log(styles)

const User = props => (
  <div className={styles.user}>
    <img src={props.avatar} className={styles.avatar} alt="" />
    <div className={styles.description}>
      <h2 className={styles.username}>{props.username}</h2>
      <p className={styles.excerpt}>{props.excerpt}</p>
    </div>
  </div>
)

export default function APage() {
  const links = [
    {
      link: 'https://github.com/abhishekkr',
      title: 'github',
    },
    {
      link: 'https://twitter.com/abionic',
      title: 'twitter',
    },
  ];

  return (
    <div style={{ color: `teal` }}>
      <Header headerText="a Page"/>
      <SubHeader>trying few more things</SubHeader>
      <div style={{ color: `purple`, fontSize: `32px` }}>who art thou</div>
      <Link to="/">home</Link>
      <br/>
      <Mylinks links={links}/>
      <br/>
      <User
        username="World"
        avatar="https://source.unsplash.com/random/400x200"
        excerpt="This is world and you are in it innit."
      />
      <User
        username="Universe"
        avatar="https://source.unsplash.com/random/400x200"
        excerpt="This is the Universe & you are a stardust."
      />
    </div>
  )
}
