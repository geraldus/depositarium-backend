import React from 'react'
import { defaultProps } from 'recompose'
import _ from 'lodash'
import 'whatwg-fetch'
import CKEditor from '@ckeditor/ckeditor5-react';
import ClassicEditor from '@ckeditor/ckeditor5-build-classic'
// import InlineEditor from '@ckeditor/ckeditor5-build-inline'
import { Formik } from 'formik'

import { Bs4FormGroupInputWithHelpContainer, Bs4FormGroupCheckboxContainer, Bs4FormGroupLabeledTextareaContainer } from '../../../bs4/containers/form/group'
import { Bs4ButtonContainer } from '../../../bs4/containers/button'

/** Bootstrap 4 Currency Form container */
export class Bs4CurrencyFormContainer extends React.Component {
    render () {
        const { props } = this
        const {
                value,
                handleChange,
                handleSubmit,
                handleBlur,
                setFieldValue,
                onSubmit,
                ...ownProps
            } = this.props
        const L = props.labels
        return (<form onSubmit={onSubmit}>
            <div className="col">
                <div className="row">
                    <Bs4FormGroupInputWithHelpContainer
                            className="col"
                            name="shortDesc"
                            value={value.shortDesc}
                            onChange={handleChange}
                            onBlur={handleBlur}>
                        {L.currency.shortDesc}
                    </Bs4FormGroupInputWithHelpContainer>
                </div>
            </div>
            <div className="col">
                <div className="row">
                    <Bs4FormGroupCheckboxContainer
                            name="isCrypto"
                            checked={value.isCrypto}
                            label={L.isCryptoCurrency}
                            onChange={handleChange}
                            onBlur={handleBlur}
                            className="col"/>
                </div>
            </div>
            <div className="col">
                <div className="row">
                    <Bs4FormGroupInputWithHelpContainer
                            className="col-3"
                            helpClassName="text-center"
                            name="sign"
                            value={value.sign}
                            onChange={handleChange}
                            onBlur={handleBlur}
                            required
                            >
                        {L.currency.sign}
                    </Bs4FormGroupInputWithHelpContainer>
                    <Bs4FormGroupInputWithHelpContainer
                            className="col-4"
                            helpClassName="text-center"
                            name="code"
                            value={value.code}
                            onChange={handleChange}
                            onBlur={handleBlur}
                            required>
                        {L.currency.code}
                    </Bs4FormGroupInputWithHelpContainer>
                    <Bs4FormGroupInputWithHelpContainer
                            className="col-5"
                            helpClassName="text-center"
                            name="ident"
                            value={value.ident}
                            onChange={handleChange}
                            onBlur={handleBlur}
                            required>
                        {L.currency.internalName}
                    </Bs4FormGroupInputWithHelpContainer>
                </div>
            </div>
            <div className="col">
                    <CKEditor
                        editor={ClassicEditor}
                        data={value.longDesc}
                        config={{
                            toolbar: ['bold', 'italic']
                        }}
                        // onInit={ editor => {
                        //     // You can store the "editor" and use when it is needed.
                        //     console.log( 'Editor is ready to use!', editor );
                        // } }
                        rows="3"
                        onChange={ ( event, editor ) => {
                            const data = editor.getData()
                            // console.log( { event, editor, data } )
                            setFieldValue("longDesc", data)
                        } }

                        // onBlur={() => console.log('BRUL')}
                    />

            </div>
            <div className="col mt-2">
                <div className="row">
                    {typeof props.onDismiss == "function" &&
                        <Bs4ButtonContainer
                                className="btn-link col-6"
                                disabled={props.pristine || props.pending}>
                            {L.cancelLabel}
                        </Bs4ButtonContainer>
                    }
                    {typeof props.onSubmit == "function" &&
                        <Bs4ButtonContainer
                                className="btn-primary col-6"
                                disabled={props.pristine || props.pending}>
                            {L.submitLabel}
                        </Bs4ButtonContainer>
                    }
                </div>
            </div>
        </form>)
    }
}


/** Bootstrap 4 Currency Form Component */
export class Bs4CurrencyForm extends React.Component {
    constructor (props) {
        super(props)
        const { value } = props
        this.initialValues = {
            sign: '',
            code: '',
            ident: '',
            isCrypto: false,
            shortDesc: '',
            longDesc: '',
            language: ''
        }
    }
    render () {
        const { value, onSubmit, ...ownProps } = this.props
        return (
            <Formik
                initialValues={this.initialValues}
                validate={() => ({})}
                onSubmit={e => onSubmit(e)}>
                {({ values,
                    handleSubmit,
                    handleChange,
                    handleBlur,
                    isSubmitting,
                    setFieldValue
                }) => (
                    <Bs4CurrencyFormContainer
                        { ...ownProps }
                        value={values}
                        onSubmit={handleSubmit}
                        handleChange={handleChange}
                        handleBlur={handleBlur}
                        setFieldValue={setFieldValue}
                        isSubmitting={isSubmitting}
                        />
                )}
            </Formik>
        )
    }
}

const withDeafaultProps = defaultProps({
    labels: {
        currency: {
            shortDesc: 'Короткое описание',
            sign: 'Знак',
            code: 'Код',
            internalName: 'Ярлык',
            longDesc: 'Длинное описание',
        },
        isCryptoCurrency: 'Криптовалюта',
        submitLabel: 'Создать валюту',
        cancelLabel: 'Отмена'
    }
})

export const DeafaultBs4CurrencyForm = withDeafaultProps(Bs4CurrencyForm)

