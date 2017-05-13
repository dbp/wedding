<apply template="_base">

    <k/>

    <confirmed>CONFIRMED</confirmed>

    <table>
    <dfForm ref="rsvp">

        <tr>
            <td>
        <dfLabel ref="friday">
            Friday (Sept 15) Night
            <dfInputCheckbox ref="friday"/>
        </dfLabel>
            </td>
        </tr>

        <tr>
            <td>
        <dfLabel ref="saturday">
            Saturday (Sept 16) Night
            <dfInputCheckbox ref="saturday" />
        </dfLabel>
            </td>
        </tr>

        <tr>
            <td>
                <dfLabel ref="lodging">
                    Housing preferences
                    <dfInputSelect ref="lodging" />
                </dfLabel>
            </td>
        </tr>

        <tr>
            <td>
                <dfLabel ref="food">
                    Food allergies / Restrictions
                    <dfInputText ref="food"/>
                </dfLabel>
            </td>
        </tr>

        <people>
            <tr>
                <td>
                    <locked>
                        <dfLabel ref="person-${id}-include">
                            <name/>
                            <dfInputCheckbox ref="person-${id}-include"/>
                        </dfLabel>
                    </locked>
                    <not-locked> 
                        <dfLabel ref="person-${id}-name">
                            <dfInputText ref="person-${id}-name"/>
                            <dfInputCheckbox ref="person-${id}-include"/>
                        </dfLabel>
                    </not-locked>
                </td>
            </tr>
        </people>

        <tr>
            <td>
                <dfInputSubmit value="Save"/>
            </td>
        </tr>

    </dfForm>

</apply>
