import React from "react"
import { Link } from "gatsby"
import Header from "../components/header"
import SubHeader from "../components/subheader"

export default function Home() {
  return (
    <div style={{ color: `teal` }}>
      <Header headerText="Home"/>
      <SubHeader>This is Gatsby!</SubHeader>
      <div style={{ color: `purple`, fontSize: `32px` }}>who art thou</div>
      <Link to="/a-page">let's check</Link><br/>
      <img src="https://source.unsplash.com/random/400x200" alt="" />
    </div>
  )
}
