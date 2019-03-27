
import React from 'react'
import { defaultProps } from 'recompose'

import NavBar from './nav-bar'

const CollapseNav = props => {
    const { items, labels, menuId } = props
    const L = labels
    const classes = [
            "collapse",
            "navbar-collapse",
            props.className,
        ].join(' ')
    return (<React.Fragment>
        <button
                className="navbar-toggler"
                type="button"
                data-toggle="collapse"
                data-target={`#${menuId}`}
                aria-controls={menuId}
                aria-expanded="false"
                aria-label={L.ariaExpandMenu} >
            <span className="navbar-toggler-icon"/>
        </button>
        <div id={props.menuId} className={classes}>
            <NavBar items={items} renderItem={props.renderItem}/>
        </div>
        </React.Fragment>)
}

const withDeafaultProps = defaultProps({
    labels: {
        ariaExpandMenu: 'Раскрыть меню сайта'
    },
    className: "justify-content-between",
    itemClassName: undefined,
    renderItem: props => <NavItem { ... props } />
})

export default withDeafaultProps(CollapseNav)