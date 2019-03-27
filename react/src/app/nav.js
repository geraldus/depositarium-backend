import React from 'react'
import { defaultProps } from 'recompose'

import CollapseNav from './collapse-nav'
import NavBar from './nav-bar'

const AppNav = (props) => {
    const { items, menuId } = props
    const hasStyle = typeof props.style != typeof undefined
    return (
        <nav className={props.navClassNames} style={hasStyle? props.style : undefined}>
            {items.collapsible.length > 0 &&
                <CollapseNav
                    items={items.collapsible}
                    menuId={menuId}
                    renderItem={props.renderItem}
                    labels={props.labels}
                />
            }
            {typeof items.sticky.right != typeof undefined &&
                <NavBar
                    items={items.sticky.right}
                    renderItem={props.renderItem}/>}
        </nav>
    )
}

const withDeafaultProps = defaultProps({
    navClassNames: [
        'navbar',
        'sticky-top',
        'navbar-light',
        'bg-primary',
        'navbar-expand-lg'
    ].join(' '),
    collapseClasses: [
        "collapse",
        "navbar-collapse",
        "justify-content-between"
    ].join(' '),
    menuId: 'mao-app-nav',
    labels: {
        ariaExpandMenu: 'Раскрыть меню сайта'
    },
    items: {
        collapsible: [],
        sticky: {
            right: []
        }
    },
    renderItem: props => <NavItem { ...props }/>
})

export default withDeafaultProps(AppNav)