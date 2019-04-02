import React from 'react'


/** Bootstrap 4 Button container */
export class Bs4ButtonContainer extends React.Component {
    render () {
        const { props } = this
        const className = (props.className || '')
                .split(' ')
                .concat(["btn"])
                .join(' ')
        return (
            <button {...props} className={className}>
                {props.children}
            </button>
        )
    }
}
