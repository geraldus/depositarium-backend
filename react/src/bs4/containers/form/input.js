import React from 'react'

/** Bootstrap 4 Input container */
export class Bs4InputContainer extends React.Component {
    render () {
        const { props } = this
        const className = (props.className || '')
                .split(' ')
                .concat(["form-control"])
                .join(' ')
        return (
            <input {...props} className={className}/>
        )
    }
}

/** Bootstrap 4 Textarea container */
export class Bs4TextareaContainer extends React.Component {
    render () {
        const { props } = this
        const { className, ...ownProps } = props
        const className_ = (className || '')
                .split(' ')
                .concat(["form-control"])
                .join(' ')
        return (
            <textarea {...ownProps} className={className_}>
                {/* React suggests use defaultValue prop {props.children} */}
            </textarea>
        )
    }
}

