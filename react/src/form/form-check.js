import React from 'react'
import { defaultProps } from 'recompose'
import _ from 'lodash'


export class FormCheck extends React.Component {
    constructor (props) {
        super(props)
        this.state = {
            value: ''
        }
    }
    updateValue (e) {
        console.log(e)
        this.setState(s => _.metge({}, s, { value: e.value }))
        this.props.onChange(e)
    }
    render () {
        const { id, label, name, type } = this.props
        return (
            <div className="form-check">
                <input
                    id={name? `${id}-${name}` : undefined}
                    className="form-check-input"
                    type={type}
                    name={name? name : undefined}
                    value={this.state.value}
                    onChange={e => this.updateValue(e)} />
                {label && <label
                    htmlFor={`${id}-name`}
                    className="form-check-label">{label}</label>}
            </div>
        )
    }
}

const withDefaultProps = defaultProps({
    id: _.uniqueId(),
    name: ''
})

export default withDefaultProps(FormCheck)
