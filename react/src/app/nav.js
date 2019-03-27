import React from 'react'
import { defaultProps } from 'recompose'

import CollapseNav from './collapse-nav'
import NavBar from './nav-bar'

const AppNav = (props) => {
    const { items, menuId } = props
    const l = props.labels
    const hasStyle = typeof props.style != typeof undefined
    return (
        <nav className={props.navClassNames}>
            {items.collapsible.length > 0 && <React.Fragment>
                <button
                    className="navbar-toggler"
                    type="button"
                    data-toggle="collapse"
                    data-target={`#${menuId}`}
                    aria-controls={menuId}
                    aria-expanded="false"
                    aria-label={l.ariaExpandMenu}
                    style={hasStyle? props.style : undefined}>
                    <span className="navbar-toggler-icon"/>
                </button>
                <CollapseNav
                    items={items.collapsible}
                    menuId={menuId}
                />
            </React.Fragment>}
            {typeof items.sticky.right != typeof undefined &&
                <NavBar items={items.sticky.right}/>}
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
    }
})

export default withDeafaultProps(AppNav)