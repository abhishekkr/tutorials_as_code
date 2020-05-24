import React from "react"
import mylinksStyle from "./mylinks.module.css"

export default function Mylinks(props) {
  return <div className={mylinksStyle.links}>
    {props.links.map(item => (
      <span key={item.title} className={mylinksStyle.link}><a href={item.link}>{item.title}</a></span>
    ))}
  </div>
}
