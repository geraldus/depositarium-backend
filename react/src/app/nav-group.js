import React from 'react'
import { defaultProps } from 'recompose'


const NavGroup = props => {
    const classes = [
            "nav-item",
            "dropdown",
            props.className,
            ... props.active? "active" : ""
        ].join(' ')
    const idGroup = `nav-dropdown-${props.index}`
    const { items } = props
    console.log(props)
    return (
        <li id={idGroup} className={classes}>
            <a
                    id={`${idGroup}-toggle`}
                    className="nav-link dropdown-toggle"
                    data-toggle="dropdown"
                    href="#"
                    role="button"
                    aria-expanded="false"
                    aria-haspopup="true"
                    >
                {props.title}
            </a>
            <div
                    className="dropdown-menu"
                    aria-labelledby={`#${idGroup}-toggle`}>
                {items.map((item, idx) => (
                    <a
                            key={idx}
                            className="dropdown-item"
                            href={item.url}
                    >
                        {item.label}
                    </a>)
                )}
            </div>
        </li>
    )
}

const withDeafaultProps = defaultProps({
    active: false,
    className: "mx-2"
})

export default withDeafaultProps(NavGroup)