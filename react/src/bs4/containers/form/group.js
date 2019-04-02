import React from 'react'

import { Bs4TextareaContainer, Bs4InputContainer } from './input'
import { Bs4FormCheckLabeledCheckboxContainer } from './check'


/** Bootstrap 4 Form Group container */
export class Bs4FormGroupContainer extends React.Component {
    render () {
        const { props } = this
        return (<div { ...props }>
            {props.children}
        </div>)
    }
}

/** Bootstrap 4 Textarea with label within Form Group container
 */
export class Bs4FormGroupLabeledTextareaContainer extends React.Component {
    render () {
        const { props } = this
        const {
                id,
                labelClassName,
                name,
                ...ownProps
            } = props
        const className = (props.className || '')
                .split(' ')
                .concat(["form-group"])
                .join(' ')
        const labelClassName_ = (labelClassName || undefined)
        return (
            <Bs4FormGroupContainer
                    id={`${id}-wrap`}
                    className={className}>
                <label
                        htmlFor={id}
                        className={labelClassName_}>
                    {props.label}
                </label>
                <Bs4TextareaContainer
                    id={id}
                    name={name}
                    { ... ownProps}>
                    {/* React suggests use defaultValue prop {props.children} */}
                </Bs4TextareaContainer>
            </Bs4FormGroupContainer>
        )
    }
}

/** Bootstrap 4 labeless Form Group with Input and Help Text container
 *  An input wrapped in form group with help text, but without label
 */
export class Bs4FormGroupInputWithHelpContainer extends React.Component {
    render () {
        const { props } = this
        const {
                id,
                className,
                helpClassName,
                name,
                value,
                required,
                onChange,
                onBlur,
                ...ownProps
            } = props
        const className_ = (className || '')
                .split(' ')
                .concat(["form-group"])
                .join(' ')
        const helpClassName_ = (helpClassName || '')
                .split(' ')
                .concat(["form-text"])
                .join(' ')
        const helpId = `${props.id}-help`
        return (
        <Bs4FormGroupContainer
                id={`${props.id}-wrap`}
                className={className_}
                { ... ownProps} >
            <Bs4InputContainer
                    id={id}
                    aria-describedby={helpId}
                    name={name}
                    value={value}
                    onChange={onChange}
                    required={required}
                    onBlur={onBlur}/>
            <small
                    id={helpId}
                    htmlFor={id}
                    className={helpClassName_}>
                {props.children}
            </small>
        </Bs4FormGroupContainer>)
    }
}

/** Bootstrap 4 Form Check with Checbox wrapped by Form Group */
export class Bs4FormGroupCheckboxContainer extends React.Component {
    render () {
        const {
                id,
                label,
                name,
                value,
                checked,
                onChange,
                onBlur,
                ...ownProps
            } = this.props
        return (
            <Bs4FormGroupContainer id={id} { ...ownProps }>
                <Bs4FormCheckLabeledCheckboxContainer
                        id={`${id}-check`}
                        label={label}
                        name={name}
                        value={value}
                        checked={checked}
                        onChange={onChange}
                        onBlur={onBlur}
                        { ...ownProps }/>
            </Bs4FormGroupContainer>
        )
    }
}
