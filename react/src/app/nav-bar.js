
import React from 'react'
import { defaultProps } from 'recompose'

import NavItem from './nav-item'

const NavBar = props => {
    const { items } = props
    const classes = [
            "navbar-nav",
            props.className,
        ].join(' ')
    return (
        <ul className={classes}>
            {items.map((item, idx) =>
                <NavItem
                    key={idx}
                    className={props.itemClassName}
                    href={item.value.url}
                    label={item.value.label}/>)}
        </ul>
    )
}

const withDeafaultProps = defaultProps({
    itemClassName: undefined
})

export default withDeafaultProps(NavBar)