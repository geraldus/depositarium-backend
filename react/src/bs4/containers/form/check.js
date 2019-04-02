import React from 'react'


/** Bootstrap 4 Form Check wrapper container */
export class Bs4FormCheckContainer extends React.Component {
    render () {
        const { props } = this
        const className = (props.className || '')
                .split(' ')
                .concat(["form-check"])
                .join(' ')
        return (
            <div { ...props } className={className}>
                {props.children}
            </div>
        )
    }
}

/** Bootstrap 4 Form Check container
 *  Contains configurable input with label
 *
 */
export class Bs4FormCheckLabeledInputContainer extends React.Component {
    render () {
        const { props } = this
        const
            {
                id,
                type,
                name,
                label,
                className,
                checked,
                value,
                onChange,
                onBlur,
                ...ownProps
            } = props
        const className_ = (className || undefined)
        return (
            <Bs4FormCheckContainer
                    id={`${id}-wrap`}
                    className={className_}
                    { ...ownProps }>
                <input
                        id={id}
                        className="form-check-input"
                        type={type}
                        name={name? name : undefined}
                        value={value}
                        defaultChecked={checked}
                        onChange={onChange}
                        onBlur={onBlur}/>
                    {label && <label
                        htmlFor={`${id}-input`}
                        className="form-check-label">{label}</label>}
            </Bs4FormCheckContainer>
        )
    }
}

/** Bootstrap 4 Form Check labeled Checkbox container */
export class Bs4FormCheckLabeledCheckboxContainer extends React.Component {
    render () {
        const { props } = this
        const { className, ...ownProps } = props
        const className_ = (props.className || undefined)
        return (
                <Bs4FormCheckLabeledInputContainer
                        className={className_}
                        type="checkbox"
                        { ...ownProps}
                    />
        )
    }
}