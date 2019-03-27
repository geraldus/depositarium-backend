import React from 'react'
import { defaultProps } from 'recompose'


const NavItem = props => {
    const classes = [
            "nav-item",
            props.className,
            ... props.active? "active" : ""
        ].join(' ')
    return (
        <li className={classes}>
            <a className="nav-link" href={props.href}>{props.label}</a>
        </li>
    )
}

const withDeafaultProps = defaultProps({
    active: false,
    className: "mx-2"
})

export default withDeafaultProps(NavItem)