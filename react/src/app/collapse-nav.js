
import React from 'react'
import { defaultProps } from 'recompose'

import NavBar from './nav-bar'

const CollapseNav = props => {
    const { items } = props
    const classes = [
            "collapse",
            "navbar-collapse",
            props.className,
        ].join(' ')
    return (
        <div id={props.menuId} className={classes}>
            <NavBar items={items} />
        </div>
    )
}

const withDeafaultProps = defaultProps({
    className: "justify-content-between",
    itemClassName: undefined
})

export default withDeafaultProps(CollapseNav)