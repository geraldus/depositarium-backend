
import React from 'react'
import { defaultProps } from 'recompose'

import NavItem from './nav-item'
import NavGroup from './nav-group'

const NavBar = props => {
    const { items } = props
    const classes = [
            "navbar-nav",
            props.className,
        ].join(' ')
    return (
        <ul className={classes}>
            {items.map((item, idx) => {
                if (item.type == 'single') {
                    return (<NavItem
                        key={idx}
                        index={idx}
                        className={props.itemClassName}
                        href={item.value.url}
                        label={item.value.label}/>)
                } else {
                    return (<NavGroup
                        key={idx}
                        index={idx}
                        items={item.items}
                        title={item.title}/>)
                }
            })}
        </ul>
    )
}

const withDeafaultProps = defaultProps({
    itemClassName: undefined
})

export default withDeafaultProps(NavBar)